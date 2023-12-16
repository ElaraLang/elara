{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Elara.Emit.Expr where

import Control.Lens
import Data.List (foldl)
import Data.Text qualified as Text
import Data.Traversable (for)
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core as Core
import Elara.Data.Unique
import Elara.Emit.Error
import Elara.Emit.Lambda
import Elara.Emit.Operator
import Elara.Emit.Params
import Elara.Emit.State (MethodCreationState (thisClassName), findLocalVariable, withLocalVariableScope)
import Elara.Emit.Utils
import Elara.Emit.Var
import Elara.Prim.Core
import Elara.ToCore (stripForAll)
import Elara.Utils (uncurry3)
import GHC.Records (HasField)
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
import Print (debugColored, debugPretty, showPretty)

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
generateInstructions' (Var (JVMLocal 0)) _ = emit $ ALoad 0
generateInstructions' (Var (JVMLocal 1)) _ = emit $ ALoad 1
generateInstructions' (Var (JVMLocal 2)) _ = emit $ ALoad 2
generateInstructions' (Var (JVMLocal 3)) _ = emit $ ALoad 3
generateInstructions' (Lit s) _ = generateLitInstructions s >>= emit'
generateInstructions' (Var (Normal (Id (Global' v) _))) _
    | v == fetchPrimitiveName = error "elaraPrimitive without argument"
generateInstructions' (App ((Var (Normal (Id (Global' v) _)))) (Lit (String primName))) _
    | v == fetchPrimitiveName = generatePrimInstructions primName >>= emit'
generateInstructions' (App (TyApp (Var (Normal (Id (Global (Identity v)) _))) _) (Lit (String primName))) _
    | v == fetchPrimitiveName = generatePrimInstructions primName >>= emit'
generateInstructions' v@(Var (Normal (Id (Global' qn@(Qualified n mn)) t))) tApps = do
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
                inst <- etaExpand v (foldl' instantiate t tApps) cName
                emit inst
            _ -> do
                -- no args function (eg undefined)
                emit
                    ( InvokeStatic
                        (ClassInfoType $ createModuleName mn)
                        (translateOperatorName n)
                        (generateMethodDescriptor t)
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
    generateAppInstructions f x
generateInstructions' (Let (NonRecursive (n, val)) b) tApps = withLocalVariableScope $ do
    idx <- localVariableId n
    generateInstructions' val tApps
    emit $ AStore idx
    generateInstructions' b tApps
generateInstructions' (Match a b c) _ = generateCaseInstructions a b c
generateInstructions' (TyApp f t) tApps =
    generateInstructions' f (t : tApps) -- TODO
generateInstructions' (Lam (Normal (Id (Local' v) binderType)) body) _ = do
    cName <- gets (.thisClassName)
    inst <- createLambda (v, generateFieldType binderType) (ObjectFieldType "java/lang/Object") cName body
    emit inst
    pass
generateInstructions' (Lam (JVMLocal v) body) _ = error "Lambda with local variable as its binder"
generateInstructions' other _ = error $ "Not implemented: " <> showPretty other

generateCaseInstructions ::
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
    [ (DataAlt (DataCon trueCtorName' boolCon'), _, ifTrue)
        , (DataAlt (DataCon falseCtorName' boolCon2'), _, ifFalse)
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
    [ (DataAlt (DataCon emptyCtorName' (AppTy listCon' _)), _, ifEmpty)
        , (DataAlt (DataCon consCtorName' listCon2'), [headBind, tailBind], ifCons)
        ]
        | emptyCtorName' == emptyListCtorName
        , listCon' == listCon
        , consCtorName' == consCtorName
        , listCon2' == listCon =
            do
                generateInstructions scrutinee
                emit $ CheckCast (ClassInfoType "elara.EList")
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

                emit $ InvokeVirtual (ClassInfoType "elara.EList") "isEmpty" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean)))
                emit $ IfNe ifEmptyLabel
                -- store the cons binds into locals
                referenceScrutinee
                emit $ GetField (ClassInfoType "elara.EList") "head" (ObjectFieldType "java.lang.Object")
                headIdx <- localVariableId headBind
                emit $ AStore headIdx

                referenceScrutinee
                emit $ GetField (ClassInfoType "elara.EList") "tail" (ObjectFieldType "elara.EList")
                tailIdx <- localVariableId tailBind
                emit $ AStore tailIdx

                generateInstructions ifCons
                emit $ Goto endLabel

                emit $ Label ifEmptyLabel
                generateInstructions ifEmpty
                emit $ Label endLabel
generateCaseInstructions scrutinee bind alts = error $ "Not implemented: " <> showPretty (scrutinee, bind, alts)

localVariableId :: HasCallStack => Member (State MethodCreationState) r => JVMBinder -> Sem r U1
localVariableId (JVMLocal i) = pure i
localVariableId (Normal ((Id (Local' v) _))) = findLocalVariable v
localVariableId other = error $ "Not a local variable: " <> showPretty other

approximateTypeAndNameOf ::
    HasCallStack =>
    Expr JVMBinder ->
    Either Word8 (UnlocatedVarRef Text, Type)
approximateTypeAndNameOf (Var (Normal (Id n t))) = Right (n, t)
approximateTypeAndNameOf (Var (JVMLocal i)) = Left i
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
        Left local -> do
            Log.debug $ "Function is a local variable: " <> showPretty local
            emit $ ALoad local
            generateInstructions x
            emit $ InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
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
                            emit inst
                        else error $ "Arity mismatch: " <> show arity <> " vs " <> show (length args) <> " for f=" <> showPretty f <> "\n x=" <> showPretty x <> "\n f'=" <> showPretty f' <> "\n args=" <> showPretty args
  where
    collectArgs :: JVMExpr -> ([Type], [JVMExpr]) -> (JVMExpr, [Type], [JVMExpr])
    collectArgs (App f x) (tArgs, args) = collectArgs f (tArgs, x : args)
    collectArgs (TyApp f t) (tArgs, args) = collectArgs f (t : tArgs, args)
    collectArgs f (tArgs, args) = (f, tArgs, args)

invokeStaticVars :: HasCallStack => Member (Error EmitError) r => UnlocatedVarRef Text -> Type -> Sem r (ClassInfoType, Text, MethodDescriptor)
invokeStaticVars (Global' (Qualified fName mn)) fType =
    pure
        ( ClassInfoType $ createModuleName mn
        , translateOperatorName fName
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
        , InvokeStatic (ClassInfoType "elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "elara.IO")))
        ]
generatePrimInstructions "toString" =
    pure
        [ ALoad 0
        , InvokeVirtual (ClassInfoType "java.lang.Object") "toString" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java.lang.String")))
        ]
generatePrimInstructions "undefined" =
    pure
        [ InvokeStatic (ClassInfoType "elara.Error") "undefined" (MethodDescriptor [] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , AConstNull
        ]
generatePrimInstructions "+" =
    pure
        [ -- sum 2 java.lang.Integers using Func<Integer, Func<Integer, Integer>> elara.Int.add
          GetStatic (ClassInfoType "elara.Prelude") "add" (ObjectFieldType "elara.Func")
        , ALoad 0
        , InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , ALoad 1
        , InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , CheckCast (ClassInfoType "java.lang.Integer")
        ]
generatePrimInstructions "-" =
    pure
        [ -- minus 2 java.lang.Integers using Func<Integer, Func<Integer, Integer>> elara.Prelude.minus
          GetStatic (ClassInfoType "elara.Prelude") "minus" (ObjectFieldType "elara.Func")
        , ALoad 0
        , InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , ALoad 1
        , InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , CheckCast (ClassInfoType "java.lang.Integer")
        ]
generatePrimInstructions "*" =
    pure
        [ -- minus 2 java.lang.Integers using Func<Integer, Func<Integer, Integer>> elara.Prelude.minus
          GetStatic (ClassInfoType "elara.Prelude") "times" (ObjectFieldType "elara.Func")
        , ALoad 0
        , InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , ALoad 1
        , InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
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
        , InvokeStatic (ClassInfoType "elara.EList") "Cons" (MethodDescriptor [ObjectFieldType "java.lang.Object", ObjectFieldType "elara.EList"] (TypeReturn (ObjectFieldType "elara.EList")))
        ]
generatePrimInstructions "empty" =
    pure
        [ InvokeStatic (ClassInfoType "elara.EList") "Empty" (MethodDescriptor [] (TypeReturn (ObjectFieldType "elara.EList")))
        ]
generatePrimInstructions "listToString" =
    pure
        [ ALoad 0
        , InvokeStatic (ClassInfoType "elara.EList") "listToString" (MethodDescriptor [ObjectFieldType "elara.EList"] (TypeReturn (ObjectFieldType "java.lang.String")))
        ]
generatePrimInstructions "stringToList" =
    pure
        [ ALoad 0
        , InvokeStatic (ClassInfoType "elara.EList") "stringToList" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "elara.EList")))
        ]
generatePrimInstructions "readFile" =
    pure
        [ ALoad 0
        , InvokeStatic (ClassInfoType "elara.IO") "readFile" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "elara.IO")))
        ]
generatePrimInstructions "ioBind" =
    pure
        [ ALoad 0
        , ALoad 1
        , InvokeVirtual (ClassInfoType "elara.IO") "bind" (MethodDescriptor [ObjectFieldType "elara.Func"] (TypeReturn (ObjectFieldType "elara.IO")))
        ]
generatePrimInstructions other = error $ "Unknown elara primitive: " <> showPretty other
