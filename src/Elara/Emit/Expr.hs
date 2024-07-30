{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Elara.Emit.Expr where

import Data.Map (union, (!?))
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Traversable (for)
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core as Core
import Elara.Core.Analysis (findTyCon)
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Emit.Error
import Elara.Emit.Lambda
import Elara.Emit.Operator
import Elara.Emit.Params
import Elara.Emit.State (MethodCreationState (..), findLocalVariable, withLocalVariableScope)
import Elara.Emit.Utils
import Elara.Emit.Var
import Elara.Logging
import Elara.Prim.Core
import Elara.ToCore (lookupCtor, lookupPrimCtor, stripForAll)
import Elara.Utils (uncurry3)
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.Builder.Code (CodeBuilder, emit, emit', newLabel)
import JVM.Data.Abstract.Descriptor (
    MethodDescriptor (..),
    ReturnDescriptor (..),
 )
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type hiding (Int)
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.Raw.Types
import Polysemy
import Polysemy.Error
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log
import Polysemy.Reader
import Polysemy.State
import Print (showColored, showPretty)

generateInstructions ::
    ( HasCallStack
    , Member (State MethodCreationState) r
    , Member CodeBuilder r
    , Member ClassBuilder r
    , Member UniqueGen r
    , Member (Error EmitError) r
    , Member (Reader GenParams) r
    , Member StructuredDebug r
    ) =>
    Expr JVMBinder ->
    Sem r ()
generateInstructions v = generateInstructions' v []

generateInstructions' ::
    ( HasCallStack
    , Member (State MethodCreationState) r
    , Member CodeBuilder r
    , Member ClassBuilder r
    , Member UniqueGen r
    , Member (Error EmitError) r
    , Member (Reader GenParams) r
    , Member StructuredDebug r
    ) =>
    Expr JVMBinder ->
    [Type] ->
    Sem r ()
generateInstructions' (Var v@(JVMLocal _ _)) _ = localVariableId v >>= emit . ALoad
generateInstructions' (Lit s) _ = generateLitInstructions s >>= emit'
generateInstructions' (Var (Normal (Id (Global' v) _ _))) _
    | v == fetchPrimitiveName = error "elaraPrimitive without argument"
generateInstructions' (App ((Var (Normal (Id (Global' v) _ _)))) (Lit (String primName))) _
    | v == fetchPrimitiveName = generatePrimInstructions primName >>= emit'
generateInstructions' (App (TyApp (Var (Normal (Id (Global (Identity v)) _ _))) _) (Lit (String primName))) _
    | v == fetchPrimitiveName = generatePrimInstructions primName >>= emit'
generateInstructions' v@(Var (Normal (Id (Global' qn@(Qualified n mn)) t (Just dc)))) tApps = debugWith ("Generating instructions for data constructor: " <> showPretty v) $ do
    -- data cons always compile to methods
    -- no args function (eg undefined)
    invokeStatic <- case approximateTypeAndNameOf v of
        Just (Right (fName, fType)) -> invokeStaticVars fName fType
        Just (Left _) -> pure (ClassInfoType $ createModuleName mn, translateOperatorName n, generateMethodDescriptor t)
        Nothing -> error $ "Unknown global variable: " <> showPretty v

    emit
        ( uncurry3 InvokeStatic invokeStatic
        )
    params <- ask
    case tApps of
        [] -> pass
        [tApp]
            | params.checkCasts -> do
                let ft = fieldTypeToClassInfoType (generateFieldType tApp)
                emit $ CheckCast ft
                debug $ "Checking no-args cast for " <> showPretty v <> " with type " <> showPretty ft
            | otherwise -> debug $ "Skipping checkcast for " <> showPretty v
        _ -> error "Multiple tApps for a single value... curious..."
generateInstructions' v@(Var (Normal (Id (Global' qn@(Qualified n mn)) t _))) tApps = debugWith ("Generating instructions for global variable: " <> showPretty v) $ do
    if typeIsValue t
        then
            if
                | qn == trueCtorName -> emit $ GetStatic (ClassInfoType "java.lang.Boolean") "TRUE" (ObjectFieldType "java.lang.Boolean")
                | qn == falseCtorName -> emit $ GetStatic (ClassInfoType "java.lang.Boolean") "FALSE" (ObjectFieldType "java.lang.Boolean")
                | otherwise ->
                    emit
                        ( GetStatic
                            (ClassInfoType $ createModuleName mn)
                            (translateOperatorName n)
                            (generateFieldType t)
                        )
        else case stripForAll t of
            FuncTy{} -> do
                -- it's a method function, so we have to eta-expand it
                cName <- gets (.thisClassName)
                inst <- etaExpandN v (foldl' instantiate t tApps) cName
                emit' inst
            _ -> do
                -- no args function (eg undefined)
                invokeStatic <- case approximateTypeAndNameOf v of
                    Just (Right (fName, fType)) -> invokeStaticVars fName fType
                    Just (Left _) -> pure (ClassInfoType $ createModuleName mn, translateOperatorName n, generateMethodDescriptor t)
                    Nothing -> error $ "Unknown global variable: " <> showPretty v

                emit
                    ( uncurry3 InvokeStatic invokeStatic
                    )
                params <- ask
                case tApps of
                    [] -> pass
                    [tApp]
                        | params.checkCasts -> do
                            let ft = fieldTypeToClassInfoType (generateFieldType tApp)
                            emit $ CheckCast ft
                            debug $ "Checking no-args cast for " <> showPretty v <> " with type " <> showPretty ft
                        | otherwise -> debug $ "Skipping checkcast for " <> showPretty v
                    _ -> error "Multiple tApps for a single value... curious..."
generateInstructions' (Var v) _ = do
    idx <- localVariableId v
    emit $ ALoad idx
generateInstructions' (App f x) t = do
    generateAppInstructions f x
generateInstructions' (Let (NonRecursive (n, val)) b) tApps = withLocalVariableScope $ debugWith ("Generating let-in instructions: " <> showPretty (n, val, b)) $ do
    idx <- localVariableId n
    debug $ "Storing value in local variable " <> show idx
    generateInstructions' val tApps
    emit $ AStore idx
    generateInstructions' (replaceVar n (JVMLocal idx (jvmBinderType n)) b) tApps
generateInstructions' (Match a b c) _ = generateCaseInstructions a b c
generateInstructions' (TyApp f t) tApps =
    generateInstructions' f (t : tApps) -- TODO
generateInstructions' (Lam (Normal (Id (Local' v) binderType _)) body) _ = do
    cName <- gets (.thisClassName)
    returnType <- case approximateTypeAndNameOf body of
        Just (Right (_, t)) -> pure $ generateFieldType t
        Just (Left (_, Just t)) -> pure $ jvmLocalTypeToFieldType t
        _ -> pure $ ObjectFieldType "java/lang/Object" -- this is probably fine
    inst <- createLambda [(v, generateFieldType binderType)] returnType cName body
    emit' inst
    pass
generateInstructions' (Lam (JVMLocal _ _) _) _ = error "Lambda with local variable as its binder"
generateInstructions' other _ = error $ "Not implemented: " <> showPretty other

generateCaseInstructions ::
    forall r.
    ( Member (State MethodCreationState) r
    , Member CodeBuilder r
    , Member ClassBuilder r
    , Member UniqueGen r
    , Member (Error EmitError) r
    , Member (Reader GenParams) r
    , Member StructuredDebug r
    ) =>
    Expr JVMBinder ->
    Maybe JVMBinder ->
    [Core.Alt JVMBinder] ->
    Sem r ()
generateCaseInstructions -- hardcode if/else impl
    scrutinee
    _
    [ (DataAlt (DataCon trueCtorName' (ConTy boolCon') _), _, ifTrue)
        , (DataAlt (DataCon falseCtorName' (ConTy boolCon2') _), _, ifFalse)
        ]
        | trueCtorName' == trueCtorName && falseCtorName' == falseCtorName && boolCon' == boolCon && boolCon2' == boolCon = do
            generateInstructions scrutinee
            ifFalseLabel <- newLabel
            endLabel <- newLabel

            emit (InvokeVirtual (ClassInfoType "java.lang.Boolean") "booleanValue" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean))))
            emit' [IfEq ifFalseLabel]
            generateInstructions ifTrue
            emit' [Goto endLabel, Label ifFalseLabel]
            generateInstructions ifFalse
            emit' [Label endLabel]
generateCaseInstructions scrutinee (Just bind) alts = debugWith ("generateCaseInstructions: " <> showPretty (scrutinee, bind, alts)) $ do
    -- matches over ADTs are pretty simple, since we can just call the generated match method
    -- For example, @type Option a = None | Some a@ will have a match method that looks like:
    -- java.lang.Object match(elara.Func0 none, elara.Func some)
    -- firstly we need to find the actual type that we're matching over so we can analyse all its constructors
    let (binderType, ctors) = case jvmBinderType bind of
            Just (JVMLType x) | Just (TyCon t (TyADT ctors)) <- findTyCon x -> (t, ctors)
            x -> error $ "unknown type: " <> showColored x
    let altsMap :: Map (Qualified Text) ([JVMBinder], Expr JVMBinder) =
            fromList
                ( alts
                    <&> ( \case
                            (DataAlt (DataCon altName _ _), binders, altBody) -> (altName, (binders, altBody))
                            _ -> error "aaa"
                        )
                )
    -- We now turn each alternative into a lambda taking the binders as arguments
    -- This will be passed to Type#match which will call the appropriate lambda
    let lambdas =
            altsMap <&> \(binders :: [JVMBinder], altBody) -> do
                cName <- gets (.thisClassName)
                let returnType = ObjectFieldType "java/lang/Object"
                let binders' =
                        binders <&> \case
                            JVMLocal i t -> error "Not a local variable"
                            Normal (Id (Local' v) t _) -> (v, generateFieldType t)
                            other -> error $ "Not a local variable: " <> showPretty other
                x <- get @MethodCreationState
                generateLambda x binders' returnType cName altBody
    -- Emit the lambdas in order
    generateInstructions scrutinee
    for_ ctors $ \ctor -> do
        case lambdas !? ctor of
            Just sem -> sem
            Nothing -> error "no alt"
    -- Call the match method on the scrutinee
    let adtName = createQualifiedClassName binderType
    emit $
        InvokeVirtual
            (ClassInfoType adtName)
            "match"
            ( MethodDescriptor
                ( fmap
                    ( \ctor ->
                        let (binders, _) = altsMap Map.! ctor
                         in ObjectFieldType $ lambdaTypeName (length binders)
                    )
                    ctors
                )
                (TypeReturn (ObjectFieldType "java.lang.Object"))
            )
-- emit $ CheckCast
generateCaseInstructions scrutinee bind alts = error $ "Not implemented: " <> showPretty (scrutinee, bind, alts)

localVariableId ::
    (HasCallStack, Member (Error EmitError) r, Member (State MethodCreationState) r) =>
    JVMBinder ->
    Sem r U1
localVariableId (JVMLocal i _) = do
    s <- get
    if i < maxLocalVariables s
        then pure i
        else throw $ LocalVariableNotFound i s
localVariableId (Normal ((Id (Local' v) t Nothing))) = fst <$> findLocalVariable (v, t)
localVariableId other = error $ "Not a local variable: " <> showPretty other

{- | Attempt to figure out the name and type of an expression, returning either a Local variable in @Left@ or a Global variable in @Right@
This function is partial! (Sorry)
-}
approximateTypeAndNameOf ::
    HasCallStack =>
    Expr JVMBinder ->
    Maybe (Either (U1, Maybe JVMLocalType) (UnlocatedVarRef Text, Type))
approximateTypeAndNameOf (Var (Normal (Id n t Nothing))) = Just $ Right (n, t)
-- \| Here we turn the mention of Main.Box into Main.Wrapper.Box to reflect that it'll get compiled into a separate class
approximateTypeAndNameOf (Var (Normal (Id n t (Just (DataCon dcName dcType ((TyCon dcCon _))))))) = do
    let Qualified dcName' dcMod = dcCon
    let newModName = appendModule dcMod dcName'
    Just $ Right (Global' (Qualified (dcName ^. unqualified) newModName), t)
approximateTypeAndNameOf (Var (JVMLocal i t)) = Just $ Left (i, t)
approximateTypeAndNameOf (TyApp e t) = second (`instantiate` t) <<$>> approximateTypeAndNameOf e
approximateTypeAndNameOf other = Nothing

{- | Generate instructions for function application
This function performs arity "analysis" to avoid redundant currying when a function is "fully applied" (i.e. all arguments are provided)

For example, if we have `f : Int -> Int -> Int` and write `(f 3) 4`, no currying is necessary,
  but if we write `f 3`, we need to curry the function to get a function of type `Int -> Int`
-}
generateAppInstructions ::
    ( HasCallStack
    , Member (State MethodCreationState) r
    , Member CodeBuilder r
    , Member ClassBuilder r
    , Member UniqueGen r
    , Member (Error EmitError) r
    , Member (Reader GenParams) r
    , Member StructuredDebug r
    ) =>
    JVMExpr ->
    JVMExpr ->
    Sem r ()
generateAppInstructions f x = debugWith ("Generating instructions for function application: (" <> showPretty f <> ") " <> showPretty x) $ do
    let (f', typeArgs, args) = collectArgs f ([], [x])
    debug $ "Collected args: " <> showPretty args
    debug $ "Collected type args: " <> showPretty typeArgs

    case approximateTypeAndNameOf f' of
        Nothing -> do
            debug $ "Function is not a variable: " <> showPretty f'
            generateInstructions f'
            -- assume that this has now pushed an Elara/Func onto the stack.
            -- I _think_ we can always make this assumption as type checking won't allow us to apply an argument to a non-function
            -- but i also wouldn't be surprised if this assumption is wrong and this blows up in the future
            generateInstructions x
            emit $ InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        Just (Left (local, _)) -> do
            debug $ "Function is a local variable: " <> showPretty local
            emit $ ALoad local
            generateInstructions x
            emit $ InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        Just (Right (fName, fType)) -> do
            let arity = typeArity fType
            debug $ "Function is a global variable: " <> showPretty fName <> " with type " <> showPretty fType <> " and arity " <> show arity
            if length args == arity
                then -- yippee, no currying necessary
                do
                    genInvokeStatic args fName fType
                    -- After calling any function we always checkcast it otherwise generic functions will die
                    let instantiatedFType = foldl' instantiate fType typeArgs
                    let instantiatedReturnType = generateReturnType instantiatedFType
                    p <- ask @GenParams
                    if p.checkCasts
                        then do
                            debug $ "Checking cast for " <> showPretty f' <> "(" <> Text.intercalate ", " (showPretty <$> args) <> ")" <> " with type " <> showPretty instantiatedReturnType
                            case instantiatedReturnType of
                                TypeReturn ft -> emit $ CheckCast (fieldTypeToClassInfoType ft)
                                VoidReturn -> pass
                        else debug $ "Skipping checkcast for " <> showPretty f' <> "(" <> Text.intercalate ", " (showPretty <$> args) <> ")"
                else
                    if length args == arity - 1
                        then do
                            -- 1 layer of eta expansion required
                            cName <- gets (.thisClassName)
                            let appWithArgs = foldl' App f' args
                            inst <- etaExpand appWithArgs (FuncTy (last $ fromList (functionTypeArgs fType)) (functionTypeResult fType)) cName
                            emit' inst
                        else
                            error $
                                "Arity mismatch. Expected: "
                                    <> show arity
                                    <> ", but got: "
                                    <> show (length args)
                                    <> " for f="
                                    <> showPretty f
                                    <> " : "
                                    <> showPretty fType
                                    <> "\n x="
                                    <> showPretty x
                                    <> "\n f'="
                                    <> showPretty f'
                                    <> "\n args="
                                    <> showPretty (args, typeArgs)
  where
    collectArgs :: JVMExpr -> ([Type], [JVMExpr]) -> (JVMExpr, [Type], [JVMExpr])
    collectArgs (App f x) (tArgs, args) = collectArgs f (tArgs, x : args)
    collectArgs (TyApp f t) (tArgs, args) = collectArgs f (t : tArgs, args)
    collectArgs f (tArgs, args) = (f, tArgs, args)

invokeStaticVars ::
    HasCallStack =>
    Member (Error EmitError) r =>
    UnlocatedVarRef Text ->
    Type ->
    Sem r (ClassInfoType, Text, MethodDescriptor)
invokeStaticVars (Global' (Qualified fName mn)) fType =
    pure
        ( ClassInfoType $ createModuleName mn
        , "_" <> translateOperatorName fName
        , generateMethodDescriptor fType
        )
invokeStaticVars (Local' x) t = throw $ InvokeStaticLocal x t

genInvokeStatic args (Global' (Qualified fName mn)) fType = do
    traverse_ generateInstructions args
    emit $
        InvokeStatic
            (ClassInfoType $ createModuleName mn)
            ("_" <> translateOperatorName fName)
            (generateMethodDescriptor fType)
genInvokeStatic args (Local' x) t = do
    traverse_ generateInstructions args
    -- assume it's an Elara.Func<N>
    let name = lambdaTypeName (length args)
    -- invoke the run method on the lambda
    emit $ InvokeInterface (ClassInfoType name) "run" (MethodDescriptor (replicate (length args) (ObjectFieldType "java.lang.Object")) (TypeReturn (ObjectFieldType "java.lang.Object")))

generateLitInstructions :: Monad m => Literal -> m [Instruction]
generateLitInstructions (String s) =
    pure
        [ LDC (LDCString s)
        ]
generateLitInstructions (Int i) =
    pure
        [ LDC (LDCInt (fromIntegral i))
        , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer")))
        ]
generateLitInstructions (Core.Char c) =
    pure
        [ LDC (LDCInt (fromEnum c))
        , InvokeStatic (ClassInfoType "java.lang.Character") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Char] (TypeReturn (ObjectFieldType "java.lang.Character")))
        ]
generateLitInstructions Core.Unit =
    pure [GetStatic (ClassInfoType "Elara.Unit") "unit" (ObjectFieldType "Elara.Unit")]
generateLitInstructions other = error $ "Not implemented: " <> showPretty other

generatePrimInstructions :: Monad m => Text -> m [Instruction]
generatePrimInstructions "println" =
    pure
        [ ALoad 0
        , InvokeStatic (ClassInfoType "Elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "Elara.IO")))
        ]
generatePrimInstructions "toString" =
    pure
        [ ALoad 0
        , InvokeVirtual (ClassInfoType "java.lang.Object") "toString" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java.lang.String")))
        ]
generatePrimInstructions "undefined" =
    pure
        [ InvokeStatic (ClassInfoType "Elara.Error") "undefined" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , AConstNull
        ]
generatePrimInstructions "+" =
    pure
        [ -- sum 2 java.lang.Integers using Func<Integer, Func<Integer, Integer>> elara.Int.add
          GetStatic (ClassInfoType "Elara.Prelude") "add" (ObjectFieldType "Elara.Func")
        , ALoad 0
        , InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , ALoad 1
        , InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , CheckCast (ClassInfoType "java.lang.Integer")
        ]
generatePrimInstructions "-" =
    pure
        [ -- minus 2 java.lang.Integers using Func<Integer, Func<Integer, Integer>> elara.Prelude.minus
          GetStatic (ClassInfoType "Elara.Prelude") "minus" (ObjectFieldType "Elara.Func")
        , ALoad 0
        , InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , ALoad 1
        , InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , CheckCast (ClassInfoType "java.lang.Integer")
        ]
generatePrimInstructions "*" =
    pure
        [ -- minus 2 java.lang.Integers using Func<Integer, Func<Integer, Integer>> elara.Prelude.minus
          GetStatic (ClassInfoType "Elara.Prelude") "times" (ObjectFieldType "Elara.Func")
        , ALoad 0
        , InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , ALoad 1
        , InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , CheckCast (ClassInfoType "java.lang.Integer")
        ]
generatePrimInstructions "==" =
    pure
        [ ALoad 0
        , ALoad 1
        , InvokeStatic (ClassInfoType "java.util.Objects") "equals" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "java.lang.Object"] (TypeReturn (PrimitiveFieldType JVM.Boolean)))
        , InvokeStatic (ClassInfoType "java.lang.Boolean") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Boolean] (TypeReturn (ObjectFieldType "java.lang.Boolean")))
        ]
generatePrimInstructions "cons" =
    pure
        [ ALoad 0
        , ALoad 1
        , InvokeStatic (ClassInfoType "Elara.Prim.List") "_Cons" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "Elara.Prim.List"] (TypeReturn (ObjectFieldType "Elara.Prim.List")))
        ]
generatePrimInstructions "empty" =
    pure
        [ InvokeStatic (ClassInfoType "Elara.Prim.List") "_Nil" (MethodDescriptor [] (TypeReturn (ObjectFieldType "Elara.Prim.List")))
        ]
generatePrimInstructions "readFile" =
    pure
        [ ALoad 0
        , InvokeStatic (ClassInfoType "Elara.IO") "readFile" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "Elara.IO")))
        ]
generatePrimInstructions "ioBind" =
    pure
        [ ALoad 0
        , ALoad 1
        , InvokeVirtual (ClassInfoType "Elara.IO") "bind" (MethodDescriptor [ObjectFieldType "Elara.Func"] (TypeReturn (ObjectFieldType "Elara.IO")))
        ]
generatePrimInstructions "pure" =
    pure
        [ ALoad 0
        , InvokeStatic (ClassInfoType "Elara.IO") "pure" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "Elara.IO")))
        ]
-- consString : Char -> String -> String
generatePrimInstructions "consString" =
    pure
        [ ALoad 0
        , -- toString on the first argument
          InvokeVirtual (ClassInfoType "java.lang.Character") "toString" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java.lang.String")))
        , ALoad 1
        , -- concat the two strings
          InvokeVirtual (ClassInfoType "java.lang.String") "concat" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "java.lang.String")))
        ]
-- stringLength : String -> Int
generatePrimInstructions "stringLength" =
    pure
        [ ALoad 0
        , InvokeVirtual (ClassInfoType "java.lang.String") "length" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Int)))
        , InvokeStatic (ClassInfoType "java.lang.Integer") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.Integer")))
        ]
generatePrimInstructions "unconsString" =
    -- the rough impl equivalent here is
    {-
        return Tuple2._Tuple2(s.charAt(0), s.substring(1));
    -}
    pure
        [ ALoad 0
        , LDC (LDCInt 0)
        , InvokeVirtual (ClassInfoType "java.lang.String") "charAt" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (PrimitiveFieldType JVM.Char)))
        , InvokeStatic (ClassInfoType "java.lang.Character") "valueOf" (MethodDescriptor [PrimitiveFieldType JVM.Char] (TypeReturn (ObjectFieldType "java.lang.Character")))
        , ALoad 0
        , LDC (LDCInt 1)
        , InvokeVirtual (ClassInfoType "java.lang.String") "substring" (MethodDescriptor [PrimitiveFieldType JVM.Int] (TypeReturn (ObjectFieldType "java.lang.String")))
        , InvokeStatic (ClassInfoType "Elara.Prim.Tuple2") "_Tuple2" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "Elara.Prim.Tuple2")))
        ]
generatePrimInstructions other = error $ "Unknown elara primitive: " <> showPretty other
