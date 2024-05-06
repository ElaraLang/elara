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
import Print (debugPretty, showPretty)

generateInstructions ::
    ( HasCallStack
    , Member (State MethodCreationState) r
    , Member CodeBuilder r
    , Member ClassBuilder r
    , Member UniqueGen r
    , Member (Error EmitError) r
    , Member (Reader GenParams) r
    , Member Log r
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
    , Member Log r
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
generateInstructions' v@(Var (Normal (Id (Global' qn@(Qualified n mn)) t _))) tApps = do
    Log.debug $ "Generating instructions for global variable: " <> showPretty v
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
                    Right (fName, fType) -> invokeStaticVars fName fType
                    Left _ -> pure (ClassInfoType $ createModuleName mn, translateOperatorName n, generateMethodDescriptor t)

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
                            Log.debug $ "Checking no-args cast for " <> showPretty v <> " with type " <> showPretty ft
                        | otherwise -> Log.debug $ "Skipping checkcast for " <> showPretty v
                    _ -> error "Multiple tApps for a single value... curious..."
generateInstructions' (Var v) _ = do
    idx <- localVariableId v
    emit $ ALoad idx
generateInstructions' (App f x) t = do
    Log.debug $ "Generating instructions for function application: (" <> showPretty f <> ") " <> showPretty x <> " with type args" <> showPretty t
    generateAppInstructions f x
generateInstructions' (Let (NonRecursive (n, val)) b) tApps = withLocalVariableScope $ do
    idx <- localVariableId n
    generateInstructions' val tApps
    emit $ AStore idx
    generateInstructions' (replaceVar n (JVMLocal idx (jvmBinderType n)) b) tApps
generateInstructions' (Match a b c) _ = generateCaseInstructions a b c
generateInstructions' (TyApp f t) tApps =
    generateInstructions' f (t : tApps) -- TODO
generateInstructions' (Lam (Normal (Id (Local' v) binderType _)) body) _ = do
    cName <- gets (.thisClassName)
    returnType <- case approximateTypeAndNameOf body of
        Right (_, t) -> pure $ generateFieldType t
        Left (_, Just t) -> pure $ jvmLocalTypeToFieldType t
        Left (_, Nothing) -> error "Lambda with no return type"
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
    , Member Log r
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
generateCaseInstructions
    scrutinee
    as -- hardcode cons/empty imp
    [ (DataAlt (DataCon emptyCtorName' (AppTy (ConTy listCon') _) _), _, ifEmpty)
        , (DataAlt (DataCon consCtorName' (ConTy listCon2') _), [headBind, tailBind], ifCons)
        ]
        | emptyCtorName' == emptyListCtorName
        , listCon' == listCon
        , consCtorName' == consCtorName
        , listCon2' == listCon =
            do
                generateInstructions scrutinee
                emit $ CheckCast (ClassInfoType "Elara.EList")
                -- store the scrutinee into the as, if it exists

                as' <- for as $ \as' -> do
                    idx <- localVariableId as'
                    emit Dup
                    emit $ AStore idx
                    pure idx
                let referenceScrutinee = case as' of
                        Just as' -> emit $ ALoad as'
                        Nothing -> generateInstructions scrutinee

                ifEmptyLabel <- newLabel
                endLabel <- newLabel

                emit $ InvokeVirtual (ClassInfoType "Elara.EList") "isEmpty" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean)))
                emit $ IfNe ifEmptyLabel
                -- store the cons binds into locals
                referenceScrutinee
                emit $ GetField (ClassInfoType "Elara.EList") "head" (ObjectFieldType "java.lang.Object")
                headIdx <- localVariableId headBind
                emit $ AStore headIdx

                referenceScrutinee
                emit $ GetField (ClassInfoType "Elara.EList") "tail" (ObjectFieldType "Elara.EList")
                tailIdx <- localVariableId tailBind
                emit $ AStore tailIdx

                generateInstructions ifCons
                emit $ Goto endLabel

                emit $ Label ifEmptyLabel
                generateInstructions ifEmpty
                emit $ Label endLabel
generateCaseInstructions scrutinee (Just bind) alts = do
    -- matches over ADTs are pretty simple, since we can just call the generated match method
    -- For example, @type Option a = None | Some a@ will have a match method that looks like:
    -- java.lang.Object match(elara.Func0 none, elara.Func some)
    -- firstly we need to find the actual type that we're matching over so we can analyse all its constructors
    let (binderType, ctors) = case jvmBinderType bind of
            Just (JVMLType x) | Just (TyCon t (TyADT ctors)) <- findTyCon x -> (t, ctors)
            x -> error $ "unknown type: " <> show x
    -- undefined <- undefinedId
    -- let defaultBindersMap :: Map _ JVMExpr = fromList (ctors <&> (,Var $ Normal undefined))
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
                inst <- createLambda binders' returnType cName altBody
                emit' inst
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
                    (\(binders, altBody) -> ObjectFieldType $ lambdaTypeName (length binders))
                    (Map.elems altsMap)
                )
                (TypeReturn (ObjectFieldType "java.lang.Object"))
            )
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
localVariableId (Normal ((Id (Local' v) _ Nothing))) = findLocalVariable v
localVariableId other = error $ "Not a local variable: " <> showPretty other

{- | Attempt to figure out the name and type of an expression, returning either a Local variable in @Left@ or a Global variable in @Right@
This function is partial! (Sorry)
-}
approximateTypeAndNameOf ::
    HasCallStack =>
    Expr JVMBinder ->
    Either (U1, Maybe JVMLocalType) (UnlocatedVarRef Text, Type)
approximateTypeAndNameOf (Var (Normal (Id n t Nothing))) = Right (n, t)
-- \| Here we turn the mention of Main.Box into Main.Wrapper.Box to reflect that it'll get compiled into a separate class
approximateTypeAndNameOf (Var (Normal (Id n t (Just (DataCon dcName dcType ((TyCon dcCon _))))))) = do
    let Qualified dcName' dcMod = dcCon
    let newModName = appendModule dcMod dcName'
    Right (Global' (Qualified (dcName ^. unqualified) newModName), t)
approximateTypeAndNameOf (Var (JVMLocal i t)) = Left (i, t)
approximateTypeAndNameOf (TyApp e t) = second (`instantiate` t) <$> approximateTypeAndNameOf e
approximateTypeAndNameOf other = error $ "Don't know type of: " <> showPretty other

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
    , Member Log r
    ) =>
    JVMExpr ->
    JVMExpr ->
    Sem r ()
generateAppInstructions f x = do
    Log.debug $ "Generating instructions for function application: " <> showPretty f <> " " <> showPretty x
    let (f', typeArgs, args) = collectArgs f ([], [x])
    Log.debug $ "Collected args: " <> showPretty args
    Log.debug $ "Collected type args: " <> showPretty typeArgs

    case approximateTypeAndNameOf f' of
        Left (local, _) -> do
            Log.debug $ "Function is a local variable: " <> showPretty local
            emit $ ALoad local
            generateInstructions x
            emit $ InvokeInterface (ClassInfoType "Elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        Right (fName, fType) -> do
            let arity = typeArity fType
            Log.debug $ "Function is a global variable: " <> showPretty fName <> " with type " <> showPretty fType <> " and arity " <> show arity
            if length args == arity
                then -- yippee, no currying necessary
                do
                    insts <- invokeStaticVars fName fType
                    traverse_ generateInstructions args
                    emit $ uncurry3 InvokeStatic insts
                    -- After calling any function we always checkcast it otherwise generic functions will die
                    let instantiatedFType = foldl' instantiate fType typeArgs
                    let instantiatedReturnType = generateReturnType instantiatedFType
                    p <- ask @GenParams
                    if p.checkCasts
                        then do
                            Log.debug $ "Checking cast for " <> showPretty f' <> "(" <> Text.intercalate ", " (showPretty <$> args) <> ")" <> " with type " <> showPretty instantiatedReturnType
                            case instantiatedReturnType of
                                TypeReturn ft -> emit $ CheckCast (fieldTypeToClassInfoType ft)
                                VoidReturn -> pass
                        else Log.debug $ "Skipping checkcast for " <> showPretty f' <> "(" <> Text.intercalate ", " (showPretty <$> args) <> ")"
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

invokeStaticVars :: HasCallStack => Member (Error EmitError) r => UnlocatedVarRef Text -> Type -> Sem r (ClassInfoType, Text, MethodDescriptor)
invokeStaticVars (Global' (Qualified fName mn)) fType =
    pure
        ( ClassInfoType $ createModuleName mn
        , "_" <> translateOperatorName fName
        , generateMethodDescriptor fType
        )
invokeStaticVars (Local' x) t = throw $ InvokeStaticLocal x t

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
        , InvokeStatic (ClassInfoType "Elara.EList") "Cons" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "Elara.EList"] (TypeReturn (ObjectFieldType "Elara.EList")))
        ]
generatePrimInstructions "empty" =
    pure
        [ InvokeStatic (ClassInfoType "Elara.EList") "Empty" (MethodDescriptor [] (TypeReturn (ObjectFieldType "Elara.EList")))
        ]
generatePrimInstructions "listToString" =
    pure
        [ ALoad 0
        , InvokeStatic (ClassInfoType "Elara.EList") "listToString" (MethodDescriptor [ObjectFieldType "Elara.EList"] (TypeReturn (ObjectFieldType "java.lang.String")))
        ]
generatePrimInstructions "stringToList" =
    pure
        [ ALoad 0
        , InvokeStatic (ClassInfoType "Elara.EList") "stringToList" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "Elara.EList")))
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
generatePrimInstructions other = error $ "Unknown elara primitive: " <> showPretty other
