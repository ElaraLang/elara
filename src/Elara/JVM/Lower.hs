module Elara.JVM.Lower (lowerModule) where

import Effectful
import JVM.Data.Abstract.Descriptor (ReturnDescriptor (TypeReturn))
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type ()
import JVM.Data.Convert

import JVM.Data.Abstract.Descriptor qualified as JVM
import JVM.Data.Abstract.Type qualified as JVM

import Elara.AST.Name (unqualified)
import Elara.AST.VarRef
import Elara.Core
import Elara.Core.Generic
import Elara.Core.Module
import Elara.Core.Pretty ()
import Elara.Data.Unique
import Elara.Data.Unique.Effect (makeUnique)
import Elara.JVM.Lower.ADT
import Elara.JVM.Lower.Expr
import Elara.JVM.Lower.Function
import Elara.JVM.Lower.Monad
import Elara.JVM.Lower.Util
import Elara.Prim (PrimOp (..))
import Print (showPretty)

import Elara.Core qualified as Core
import Elara.JVM.IR qualified as IR

lowerModule :: Lower r => CoreModule CoreBind -> Eff r IR.Module
lowerModule (CoreModule name decls) = do
    let moduleClassName = moduleNameToQualifiedClassName name
    let (valueDecls, typeDecls) = partitionDecls decls
    methods <- concat <$> mapM (lowerBindToMethod moduleClassName) valueDecls
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

lowerBindToMethod :: Lower r => QualifiedClassName -> CoreBind -> Eff r [IR.Method]
lowerBindToMethod moduleClassName bind = case bind of
    NonRecursive (var, body) -> do
        lowerSingleBind moduleClassName var body

    -- recursive bindings are easy on the jvm :)
    Recursive bindings -> concat <$> mapM (uncurry $ lowerSingleBind moduleClassName) bindings

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

-- | Lower a single binding to a method(s)
lowerSingleBind ::
    Lower r =>
    QualifiedClassName ->
    Var ->
    CoreExpr ->
    Eff r [IR.Method]
lowerSingleBind _moduleClassName (Core.TyVar _) _ =
    error "Type variable cannot be bound to a method"
lowerSingleBind moduleClassName (Core.Id varRef type_ _) body = do
    let methodName = case varRef of
            Global qn -> qn ^. unqualified
            Local t -> uniqueToText identity t

    mPrim <- lowerPrimitiveBinding moduleClassName methodName type_ body
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
                    pure [buildStaticMethod methodName lambdaArgs retType blocks]
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
                    pure [buildStaticMethod methodName allArgs retType blocks]
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
    QualifiedClassName ->
    -- | The method name, i.e. the name of the binding
    Text ->
    -- | The type of the binding
    Core.Type ->
    -- | The body of the binding
    CoreExpr ->
    Eff r (Maybe [IR.Method])
lowerPrimitiveBinding currentClassName methodName type_ body =
    case body of
        Core.PrimOp op _ -> do
            let prim = IR.CorePrim op
                argTys = functionTypeArgs type_
                jvmArgs = map lowerType argTys
                jvmRet = lowerType (functionTypeResult type_)

            argNames <- replicateM (length argTys) (makeUnique "arg")
            let methodArgs = zip argNames jvmArgs

            case op of
                PrimGetArgs -> do
                    -- IO<List String>()
                    entryLabel <- makeUnique "getargs_entry"
                    condLabel <- makeUnique "getargs_cond"
                    bodyLabel <- makeUnique "getargs_body"
                    endLabel <- makeUnique "getargs_end"

                    argsVar <- makeUnique "args_arr"
                    iVar <- makeUnique "i"
                    listVar <- makeUnique "list_acc"
                    strVar <- makeUnique "raw_str"

                    let stringArrTy = JVM.ArrayFieldType elaraStrTy
                        javaObjTy = JVM.ObjectFieldType "java.lang.Object"
                        elaraStrTy = JVM.ObjectFieldType "Elara.String"
                        primInt = JVM.PrimitiveFieldType JVM.Int
                        ioTy = JVM.ObjectFieldType "Elara.IO"
                        listTy = JVM.ObjectFieldType "Elara.Prim.List"

                    -- entry block
                    -- args_arr = Elara.RuntimeSystem.getArgs()
                    -- list_acc = new Nil()
                    -- i = args_arr.length - 1
                    -- goto condLabel

                    let entryInstrs =
                            [ IR.Assign argsVar stringArrTy $
                                IR.Call (IR.InvokeStatic "Elara.RuntimeSystem" "getArgs" (JVM.MethodDescriptor [] (TypeReturn stringArrTy))) []
                            , IR.Assign listVar javaObjTy $
                                IR.New "Elara.Prim.Nil" []
                            , IR.Assign iVar primInt $
                                IR.PrimitiveIntOp IR.PrimSubtract (IR.ArrayLength (IR.LocalVar argsVar stringArrTy)) (IR.PrimitiveLitInt 1)
                            , IR.Jump condLabel
                            ]

                    -- condition block:
                    -- if (i > -1) goto bodyLabel else goto endLabel
                    let condInstrs =
                            [ IR.JumpIfPrimitiveBool
                                (IR.PrimitiveIntOp IR.PrimGT (IR.LocalVar iVar primInt) (IR.PrimitiveLitInt (-1)))
                                bodyLabel
                                endLabel
                            ]

                    -- body block:
                    -- raw_str = args_arr[i]
                    -- list_acc = new Cons(raw_str, list_acc)
                    -- i = i - 1
                    -- goto condLabel

                    let bodyInstrs =
                            [ IR.Assign strVar elaraStrTy $
                                IR.ArrayLoad (IR.LocalVar argsVar stringArrTy) elaraStrTy (IR.LocalVar iVar primInt)
                            , IR.Assign listVar javaObjTy $
                                IR.New
                                    "Elara.Prim.Cons"
                                    [ (IR.LocalVar strVar elaraStrTy, elaraStrTy)
                                    , (IR.LocalVar listVar javaObjTy, javaObjTy)
                                    ]
                            , IR.Assign iVar primInt $
                                IR.PrimitiveIntOp IR.PrimSubtract (IR.LocalVar iVar primInt) (IR.PrimitiveLitInt 1)
                            , IR.Jump condLabel
                            ]

                    -- end block:
                    -- return list_acc
                    let endInstrs =
                            [IR.Return (Just (IR.LocalVar listVar javaObjTy))]

                    let blocks =
                            [ IR.Block entryLabel entryInstrs
                            , IR.Block condLabel condInstrs
                            , IR.Block bodyLabel bodyInstrs
                            , IR.Block endLabel endInstrs
                            ]

                    let implMethodName = methodName <> "_impl"
                    let implMethod = buildStaticMethod implMethodName [] javaObjTy blocks

                    let closureExpr =
                            IR.MakeClosure
                                { closureTargetClass = currentClassName
                                , closureTargetMethod = implMethodName
                                , closureTarget = JVM.MethodDescriptor [] (TypeReturn javaObjTy)
                                , closureInterface = "Elara.Func0"
                                , capturedValues = []
                                }
                    let newIOExpr = IR.New "Elara.IO" [(closureExpr, JVM.ObjectFieldType "Elara.Func0")]
                    entry <- makeUnique "getargs_wrapper_entry"
                    let wrapperBlocks = [IR.Block entry [IR.Return (Just newIOExpr)]]
                    let wrapperMethod = buildStaticMethod methodName methodArgs ioTy wrapperBlocks
                    pure . Just $ [implMethod, wrapperMethod]
                PrimPrintln -> do
                    -- IO<Unit>(String)
                    let javaObjTy = JVM.ObjectFieldType "java.lang.Object"
                        printStreamTy = JVM.ObjectFieldType "java.io.PrintStream"
                        ioTy = JVM.ObjectFieldType "Elara.IO"
                    (argName, argTy) <- case methodArgs of
                        [arg] -> pure arg
                        _ -> error "println expects exactly 1 argument"
                    let implMethodName = methodName <> "_impl"
                    implEntry <- makeUnique "println_impl_entry"

                    let outExpr = IR.FieldRef "java.lang.System" "out" printStreamTy -- System.out
                    let printlnDesc = JVM.MethodDescriptor [javaObjTy] JVM.VoidReturn -- out.println(Object)
                    let printlnCall = IR.ExprStmt $ IR.Call (IR.InvokeVirtual outExpr "java.io.PrintStream" "println" printlnDesc) [IR.LocalVar argName argTy]
                    let unitExpr = IR.New "Elara.Prim.Unit" [] -- new Unit()
                    let implInstrs =
                            [ printlnCall
                            , IR.Return (Just unitExpr)
                            ]
                    let implMethod = buildStaticMethod implMethodName [(argName, argTy)] javaObjTy [IR.Block implEntry implInstrs]

                        closureExpr =
                            IR.MakeClosure
                                { closureTargetClass = currentClassName
                                , closureTargetMethod = implMethodName
                                , closureTarget = JVM.MethodDescriptor [argTy] (TypeReturn javaObjTy)
                                , closureInterface = "Elara.Func0"
                                , capturedValues = [(IR.LocalVar argName argTy, argTy)]
                                }
                    let newIOExpr = IR.New "Elara.IO" [(closureExpr, JVM.ObjectFieldType "Elara.Func0")]

                    wrapperEntry <- makeUnique "println_wrapper_entry"

                    let wrapperBlocks = [IR.Block wrapperEntry [IR.Return (Just newIOExpr)]]
                    let wrapperMethod = buildStaticMethod methodName methodArgs ioTy wrapperBlocks
                    pure . Just $ [implMethod, wrapperMethod]
                other -> do
                    let argExprs = [IR.LocalVar n t | (n, t) <- methodArgs]
                        primExpr = IR.PrimOp prim argExprs

                    entry <- makeUnique "prim_entry"
                    let body = [IR.Block entry [IR.Return (Just primExpr)]]
                    pure $ Just [buildStaticMethod methodName methodArgs jvmRet body]
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
