{-# LANGUAGE OverloadedRecordDot #-}

module Elara.Emit.Expr where

import Control.Lens
import Data.Traversable (for)
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core
import Elara.Data.Unique
import Elara.Emit.Operator
import Elara.Emit.State (MethodCreationState, findLocalVariable, withLocalVariableScope)
import Elara.Emit.Utils
import Elara.Emit.Var
import Elara.Prim.Core
import Elara.ToCore (stripForAll)
import Elara.Utils (uncurry3)
import JVM.Data.Abstract.Builder.Code (CodeBuilder, emit, emit', newLabel)
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type hiding (Int)
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.Raw.Types
import Polysemy
import Polysemy.State
import Print (debugColored, debugPretty, showPretty)

generateInstructions v = generateInstructions' v []

generateInstructions' :: (HasCallStack, Member (State MethodCreationState) r, Member (Embed CodeBuilder) r) => Expr JVMBinder -> [Type] -> Sem r ()
generateInstructions' (Var (JVMLocal 0)) _ = embed $ emit $ ALoad 0
generateInstructions' (Var (JVMLocal 1)) _ = embed $ emit $ ALoad 1
generateInstructions' (Var (JVMLocal 2)) _ = embed $ emit $ ALoad 2
generateInstructions' (Var (JVMLocal 3)) _ = embed $ emit $ ALoad 3
generateInstructions' (Lit s) _ = generateLitInstructions s >>= embed . emit'
generateInstructions' (Var (Normal (Id (Global' v) _))) _
    | v == fetchPrimitiveName = error "elaraPrimitive without argument"
generateInstructions' (App ((Var (Normal (Id (Global' v) _)))) (Lit (String primName))) _
    | v == fetchPrimitiveName = generatePrimInstructions primName >>= embed . emit'
generateInstructions' (App (TyApp (Var (Normal (Id (Global (Identity v)) _))) _) (Lit (String primName))) _
    | v == fetchPrimitiveName = generatePrimInstructions primName >>= embed . emit'
generateInstructions' (Var (Normal (Id (Global' (Qualified n mn)) t))) tApps = do
    if typeIsValue t
        then do
            embed $
                emit
                    ( GetStatic
                        (ClassInfoType $ createModuleName mn)
                        (translateOperatorName n)
                        (generateFieldType t)
                    )
        else -- it's a no-args method

            embed $
                emit
                    ( InvokeStatic
                        (ClassInfoType $ createModuleName mn)
                        (translateOperatorName n)
                        (generateMethodDescriptor t)
                    )
    case tApps of
        [] -> pass
        [t] -> embed $ emit $ CheckCast (fieldTypeToClassInfoType (generateFieldType t))
        _ -> error "Multiple tApps for a single value... curious..."
generateInstructions' (Var v) _ = do
    idx <- localVariableId v
    embed $ emit $ ALoad idx
generateInstructions' (App f x) _ = generateAppInstructions f x
generateInstructions' (Let (NonRecursive (n, val)) b) tApps = withLocalVariableScope $ do
    idx <- localVariableId n
    generateInstructions' val tApps
    embed $ emit $ AStore idx
    generateInstructions' b tApps
generateInstructions' (Match a b c) _ = generateCaseInstructions a b c
generateInstructions' (TyApp f t) tApps =
    generateInstructions' f (t : tApps) -- TODO
generateInstructions' other _ = error $ "Not implemented: " <> showPretty other

generateCaseInstructions ::
    (Member (State MethodCreationState) r, Member (Embed CodeBuilder) r) =>
    Expr JVMBinder ->
    Maybe JVMBinder ->
    [Elara.Core.Alt JVMBinder] ->
    Sem r ()
generateCaseInstructions -- hardcode if/else impl
    scrutinee
    _
    [ (DataAlt (DataCon trueCtorName' boolCon'), _, ifTrue)
        , (DataAlt (DataCon falseCtorName' boolCon2'), _, ifFalse)
        ]
        | trueCtorName' == trueCtorName && falseCtorName' == falseCtorName && boolCon' == boolCon && boolCon2' == boolCon = do
            generateInstructions scrutinee
            ifFalseLabel <- embed newLabel
            endLabel <- embed newLabel

            embed $ emit (InvokeVirtual (ClassInfoType "java.lang.Boolean") "booleanValue" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean))))
            embed $ emit' [IfEq ifFalseLabel]
            generateInstructions ifTrue
            embed $ emit' [Goto endLabel, Label ifFalseLabel]
            generateInstructions ifFalse
            embed $ emit' [Label endLabel]
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
                embed $ emit $ CheckCast (ClassInfoType "elara.EList")
                -- store the scrutinee into the as, if it exists

                as' <- for as $ \as' -> do
                    idx <- localVariableId as'
                    embed $ emit Dup
                    embed $ emit $ AStore idx
                    pure idx
                let referenceScrutinee = case as' of
                        Just as' -> embed $ emit $ ALoad as'
                        Nothing -> generateInstructions scrutinee

                ifEmptyLabel <- embed newLabel
                endLabel <- embed newLabel

                embed $ emit $ InvokeVirtual (ClassInfoType "elara.EList") "isEmpty" (MethodDescriptor [] (TypeReturn (PrimitiveFieldType JVM.Boolean)))
                embed $ emit $ IfNe ifEmptyLabel
                -- store the cons binds into locals
                referenceScrutinee
                embed $ emit $ GetField (ClassInfoType "elara.EList") "head" (ObjectFieldType "java.lang.Object")
                headIdx <- localVariableId headBind
                embed $ emit $ AStore headIdx

                referenceScrutinee
                embed $ emit $ GetField (ClassInfoType "elara.EList") "tail" (ObjectFieldType "elara.EList")
                tailIdx <- localVariableId tailBind
                embed $ emit $ AStore tailIdx

                generateInstructions ifCons
                embed $ emit $ Goto endLabel

                embed $ emit $ Label ifEmptyLabel
                generateInstructions ifEmpty
                embed $ emit $ Label endLabel
generateCaseInstructions scrutinee bind alts = error $ "Not implemented: " <> showPretty (scrutinee, bind, alts)

localVariableId :: HasCallStack => Member (State MethodCreationState) r => JVMBinder -> Sem r U1
localVariableId (JVMLocal i) = pure i
localVariableId (Normal ((Id (Local' v) _))) = findLocalVariable v
localVariableId other = error $ "Not a local variable: " <> showPretty other

approximateTypeAndNameOf :: HasCallStack => Expr JVMBinder -> Either Word8 (UnlocatedVarRef Text, Type)
approximateTypeAndNameOf (Var (Normal (Id n t))) = Right (n, t)
approximateTypeAndNameOf (Var (JVMLocal i)) = Left i
approximateTypeAndNameOf (TyApp e t) = second (`instantiate` t) <$> approximateTypeAndNameOf e
approximateTypeAndNameOf other = error $ "Don't know type of: " <> showPretty other

{- | Generate instructions for function application
This function performs arity "analysis" to avoid redundant currying when a function is "fully applied" (i.e. all arguments are provided)

For example, if we have `f : Int -> Int -> Int` and write `(f 3) 4`, no currying is necessary,
  but if we write `f 3`, we need to curry the function to get a function of type `Int -> Int`
-}
generateAppInstructions :: (Member (State MethodCreationState) r, Member (Embed CodeBuilder) r) => JVMExpr -> JVMExpr -> Sem r ()
generateAppInstructions f x = do
    let (f', args) = collectArgs f [x]
    case approximateTypeAndNameOf f' of
        Left local -> do
            embed $ emit $ ALoad local
            embed $ emit $ InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
            pass
        Right (fName, fType) -> do
            let arity = typeArity fType
            if length args == arity
                then -- yippee, no currying necessary
                do
                    let insts = invokeStaticVars fName fType
                    traverse_ generateInstructions args
                    embed $ emit $ uncurry3 InvokeStatic insts
                    -- After calling any function we always checkcast it otherwise generic functions will die
                    case insts ^. _3 . to (.return) of
                        TypeReturn ft -> embed $ emit $ CheckCast (fieldTypeToClassInfoType ft)
                        VoidReturn -> pass
                else error $ "Arity mismatch: " <> show arity <> " vs " <> show (length args) <> " for f=" <> showPretty f <> " x=" <> showPretty x <> ", f'=" <> showPretty f' <> ", args=" <> showPretty args
  where
    collectArgs :: JVMExpr -> [JVMExpr] -> (JVMExpr, [JVMExpr])
    collectArgs (App f x) args = collectArgs f (x : args)
    collectArgs f args = (f, args)

invokeStaticVars :: UnlocatedVarRef Text -> Type -> (ClassInfoType, Text, MethodDescriptor)
invokeStaticVars (Global' (Qualified fName mn)) fType =
    ( ClassInfoType $ createModuleName mn
    , translateOperatorName fName
    , generateMethodDescriptor fType
    )
invokeStaticVars (Local' (Unique' fn)) fType = undefined

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
generatePrimInstructions other = error $ "Unknown elara primitive: " <> showPretty other
