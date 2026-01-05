module Elara.JVM.Lower (lowerModule) where

import Effectful
import Elara.AST.Name (NameLike (nameText), unqualified)
import Elara.AST.VarRef
import Elara.Core
import Elara.Core qualified as Core
import Elara.Core.Generic
import Elara.Core.Module
import Elara.Core.Pretty ()
import Elara.Data.Unique
import Elara.Data.Unique.Effect (makeUnique)
import Elara.JVM.IR qualified as IR
import Elara.JVM.Lower.ADT
import Elara.JVM.Lower.Expr
import Elara.JVM.Lower.Function
import Elara.JVM.Lower.Monad
import Elara.JVM.Lower.Util
import Elara.Prim (fetchPrimitiveName, mkPrimQual)
import JVM.Data.Abstract.Descriptor (ReturnDescriptor (TypeReturn))
import JVM.Data.Abstract.Descriptor qualified as JVM
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.Convert
import Print (showPretty)

lowerModule :: Lower r => CoreModule CoreBind -> Eff r IR.Module
lowerModule (CoreModule name decls) = do
    let moduleClassName = moduleNameToQualifiedClassName name
    let (valueDecls, typeDecls) = partitionDecls decls
    methods <- concat <$> mapM lowerBindToMethod valueDecls
    let mainClass =
            IR.Class
                { IR.className = moduleClassName
                , IR.classSuper = jloName
                , IR.classFields = []
                , IR.classMethods = methods
                , IR.classConstructors = []
                }

    dataClasses <- concat <$> mapM lowerTypeDecl typeDecls

    pure $ IR.Module moduleClassName (mainClass : dataClasses)
  where
    partitionDecls :: [CoreDeclaration bind] -> ([bind], [CoreTypeDecl])
    partitionDecls = foldr step ([], [])
      where
        step (CoreValue b) (vs, ts) = (b : vs, ts)
        step (CoreType t) (vs, ts) = (vs, t : ts)

lowerBindToMethod :: Lower r => CoreBind -> Eff r [IR.Method]
lowerBindToMethod bind = case bind of
    NonRecursive (var, body) -> do
        method <- lowerSingleBind var body
        pure [method]

    -- recursive bindings are easy on the jvm :)
    Recursive bindings -> do
        mapM (uncurry lowerSingleBind) bindings

-- | Helper function to build a static method
buildStaticMethod ::
    -- | Method name
    Text ->
    -- | Arguments (name and type)
    [(Unique Text, JVM.FieldType)] ->
    -- | Return type
    JVM.FieldType ->
    -- | Method body
    [IR.Block] ->
    IR.Method
buildStaticMethod name args retType body =
    IR.Method
        { IR.methodName = name
        , IR.methodDesc = JVM.MethodDescriptor (map snd args) (TypeReturn retType)
        , IR.methodArgs = args
        , IR.methodBody = body
        , IR.methodIsStatic = True
        }

-- | Lower a single binding to a method
lowerSingleBind ::
    Lower r =>
    Var ->
    CoreExpr ->
    Eff r IR.Method
lowerSingleBind (Core.TyVar _) _ =
    error "Type variable cannot be bound to a method"
lowerSingleBind (Core.Id varRef type_ _) body = do
    let methodName = case varRef of
            Global qn -> qn ^. unqualified
            Local t -> uniqueToText identity t

    mPrim <- lowerPrimitiveBinding methodName type_ body
    case mPrim of
        Just primMethod -> pure primMethod
        Nothing -> do
            (lambdaArgs, lambdaBody) <- flattenLambda body
            let typeArgs = functionTypeArgs type_
            let retType = lowerType (functionTypeResult type_)

            case analyzeCallStrategy (length lambdaArgs) (length typeArgs) of
                DirectCall -> do
                    -- Fully saturated: lower body directly
                    (resultExpr, (mainInstrs, extraBlocks)) <- captureInstructions (lowerExpr lambdaBody)
                    entryLabel <- makeUnique "entry"
                    let blocks = buildMethodBody entryLabel mainInstrs extraBlocks (IR.Return (Just resultExpr))
                    pure $ buildStaticMethod methodName lambdaArgs retType blocks
                CreateClosure -> do
                    -- create a closure that takes the remaining args
                    let remainingArgTys = drop (length lambdaArgs) typeArgs
                    newArgs <- forM remainingArgTys $ \ty -> do
                        name <- makeUnique "arg"
                        pure (name, lowerType ty)
                    let allArgs = lambdaArgs <> newArgs

                    -- Lower body and call closure with the new args
                    (finalExpr, (mainInstrs, extraBlocks)) <- captureInstructions $ do
                        funExpr <- lowerExpr lambdaBody
                        if null newArgs
                            then pure funExpr
                            else do
                                let argExprs = [IR.LocalVar n t | (n, t) <- newArgs]
                                let callable =
                                        CallableInfo
                                            { callableTarget = InstanceMethod funExpr (erasedMethodDescriptor (length newArgs))
                                            , callableReturnType = retType
                                            , callableArity = length newArgs
                                            }
                                lowerCallable callable argExprs AsCall

                    entryLabel <- makeUnique "entry"
                    let blocks = buildMethodBody entryLabel mainInstrs extraBlocks (IR.Return (Just finalExpr))
                    pure $ buildStaticMethod methodName allArgs retType blocks
                OverApplication ->
                    error $ "More lambdas than type arguments for " <> methodName

{- | Create blocks for method body, ensuring the entry block is correctly set up.
If there are extra blocks, the entry block will jump to the first extra block.

As an example, given:
    mainInstrs = [inst1, inst2]
    extraBlocks = [blockA, blockB]
    returnInstr = returnInst

This will produce:
    [ Block entryLabel [inst1, inst2, Jump blockALabel]
    , blockA
    , Block blockBLabel [ ... , returnInst ]
-}
buildMethodBody ::
    -- | Label for the entry block
    Unique Text ->
    -- | Main instructions for the entry block
    [IR.Instruction] ->
    -- | Extra blocks
    [IR.Block] ->
    -- | Return instruction
    IR.Instruction ->
    -- | Resulting blocks
    [IR.Block]
buildMethodBody entryLabel mainInstrs extraBlocks returnInstr =
    let entryBlock = IR.Block entryLabel $ case extraBlocks of
            [] -> mainInstrs ++ [returnInstr]
            (first : _)
                | null mainInstrs -> [IR.Jump (IR.blockLabel first)]
                | otherwise -> mainInstrs
        appendReturn block = block{IR.instrs = IR.instrs block ++ [returnInstr]}
     in case nonEmpty extraBlocks of
            Nothing -> [entryBlock]
            Just neBlocks -> entryBlock : init neBlocks ++ [appendReturn (last neBlocks)]

-- | Lower a primitive binding into a method if applicable
lowerPrimitiveBinding ::
    Lower r =>
    -- | The method name, i.e. the name of the binding
    Text ->
    -- | The type of the binding
    Core.Type ->
    -- | The body of the binding
    CoreExpr ->
    Eff r (Maybe IR.Method)
lowerPrimitiveBinding methodName type_ body =
    case body of
        Core.App fun (Core.Lit (Core.String key)) ->
            -- elaraPrimitive "primName"
            case stripTyApps fun of
                Core.Var (Core.Id (Global primName) _ _)
                    | primName == mkPrimQual (nameText fetchPrimitiveName)
                    , Just prim <- primitiveFromKey key -> do
                        let argTys = functionTypeArgs type_
                            jvmArgs = map lowerType argTys
                            jvmRet = lowerType (functionTypeResult type_)

                        argNames <- replicateM (length argTys) (makeUnique "arg")
                        let methodArgs = zip argNames jvmArgs
                            argExprs = [IR.LocalVar n t | (n, t) <- methodArgs]
                            primExpr = IR.PrimOp prim argExprs

                        entry <- makeUnique "prim_entry"
                        let body = [IR.Block entry [IR.Return (Just primExpr)]]
                        pure . Just $ buildStaticMethod methodName methodArgs jvmRet body
                Core.Var (Core.Id (Global primName) _ _)
                    | primName == mkPrimQual (nameText fetchPrimitiveName) ->
                        error $ "Unknown primitive key in elaraPrimitive: " <> show key
                _ -> pure Nothing
        _ -> pure Nothing

-- | Flatten nested lambdas into a list of arguments and the final body expression
flattenLambda :: Lower r => CoreExpr -> Eff r ([(Unique Text, JVM.FieldType)], CoreExpr)
flattenLambda (Core.Lam b body) = do
    (restArgs, finalBody) <- flattenLambda body
    case b of
        Core.Id (Local name) type_ _ -> do
            let argType = lowerType type_
            pure ((name, argType) : restArgs, finalBody)
        _ -> error $ "Lambda binder must be a Local ID, found: " <> showPretty b
flattenLambda expr = pure ([], expr)

lowerTypeDecl :: Lower r => CoreTypeDecl -> Eff r [IR.Class]
lowerTypeDecl (CoreTypeDecl name _ _ typeBody) =
    case typeBody of
        CoreTypeAlias _ -> pure []
        CoreDataDecl _ dataCons -> do
            let baseClassName = qualifiedTextToClass name

            case dataCons of
                -- if there's only one constructor, we can skip the base class
                [singleCon] -> do
                    -- so it extends Object directly
                    conClass <- lowerDataCon jloName singleCon
                    pure [conClass]

                -- otherwise we create a base class and have constructors extend it
                _ -> do
                    let constructorCode =
                            [ IR.Super jloName []
                            , IR.Return Nothing
                            ]
                    constructorLabel <- makeUnique "base_constructor_entry"
                    let constructor =
                            IR.Constructor
                                { IR.constructorDesc = JVM.MethodDescriptor [] JVM.VoidReturn
                                , IR.constructorArgs = []
                                , IR.constructorBody =
                                    [IR.Block constructorLabel constructorCode]
                                }

                    let baseClass =
                            IR.Class
                                { IR.className = baseClassName
                                , IR.classSuper = jloName
                                , IR.classFields = []
                                , IR.classMethods = []
                                , IR.classConstructors =
                                    [ constructor
                                    ]
                                }

                    conClasses <- mapM (lowerDataCon baseClassName) dataCons
                    pure (baseClass : conClasses)

primitiveFromKey :: Text -> Maybe IR.PrimOp
primitiveFromKey key =
    case key of
        "+" -> Just IR.IntAdd
        "-" -> Just IR.IntSubtract
        "*" -> Just IR.IntMultiply
        "negate" -> Just IR.IntNegate
        "println" -> Just IR.Println
        "stringCons" -> Just IR.StringCons
        "stringHead" -> Just IR.StringHead
        "stringIsEmpty" -> Just IR.StringIsEmpty
        "stringTail" -> Just IR.StringTail
        "toString" -> Just IR.ToString
        "==" -> Just IR.PrimEquals
        "compare" -> Just IR.PrimCompare
        ">>=" -> Just IR.IOBind
        "error" -> Just IR.ThrowError
        "debugWithMsg" -> Just IR.DebugWithMsg
        _ -> Nothing

stripTyApps :: CoreExpr -> CoreExpr
stripTyApps (Core.TyApp e _) = stripTyApps e
stripTyApps e = e
