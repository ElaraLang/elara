module Elara.Emit.Expr where

import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Core
import Elara.Data.Unique
import Elara.Emit.Operator
import Elara.Emit.Utils
import Elara.Emit.Var
import Elara.Prim.Core
import Elara.Utils (uncurry3)
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type hiding (Int)
import JVM.Data.Abstract.Type qualified as JVM
import Print (showPretty)

generateInstructions :: (HasCallStack, Monad m) => Expr JVMBinder -> ClassBuilderT m [Instruction]
generateInstructions (Var (JVMLocal 0)) = pure [ALoad0]
generateInstructions (Var (JVMLocal 1)) = pure [ALoad1]
generateInstructions (Var (JVMLocal 2)) = pure [ALoad2]
generateInstructions (Var (JVMLocal 3)) = pure [ALoad3]
generateInstructions (Lit s) = generateLitInstructions s
generateInstructions (Var (Normal (Id (Global' v) _)))
    | v == fetchPrimitiveName = error "elaraPrimitive without argument"
generateInstructions (App ((Var (Normal (Id (Global' v) _)))) (Lit (String primName)))
    | v == fetchPrimitiveName = generatePrimInstructions primName
generateInstructions (App (TyApp (Var (Normal (Id (Global (Identity v)) _))) _) (Lit (String primName)))
    | v == fetchPrimitiveName = generatePrimInstructions primName
generateInstructions (Var (Normal (Id (Global' (Qualified n mn)) t))) =
    pure
        [ GetStatic
            (ClassInfoType $ createModuleName mn)
            (translateOperatorName n)
            (generateFieldType t)
        ]
generateInstructions (App f x) = generateAppInstructions f x
generateInstructions other = error $ "Not implemented: " <> showPretty other

approximateTypeAndNameOf :: Expr JVMBinder -> (UnlocatedVarRef Text, Type)
approximateTypeAndNameOf (Var (Normal (Id n t))) = (n, t)
approximateTypeAndNameOf (TyApp e t) = second (`instantiate` t) (approximateTypeAndNameOf e)
approximateTypeAndNameOf other = error $ "Don't know type of: " <> showPretty other

{- | Generate instructions for function application
This function performs arity "analysis" to avoid redundant currying when a function is "fully applied" (i.e. all arguments are provided)

For example, if we have `f : Int -> Int -> Int` and write `(f 3) 4`, no currying is necessary,
  but if we write `f 3`, we need to curry the function to get a function of type `Int -> Int`
-}
generateAppInstructions :: (Monad m) => JVMExpr -> JVMExpr -> ClassBuilderT m [Instruction]
generateAppInstructions f x = do
    let (f', args) = collectArgs f [x]
    let (fName, fType) = approximateTypeAndNameOf f'

    let arity = typeArity fType
    if length args == arity
        then -- yippee, no currying necessary
        do
            let insts = invokeStaticVars fName fType
            xInsts <- join <$> traverse generateInstructions args
            pure $ xInsts <> [uncurry3 InvokeStatic insts]
        else error $ "Arity mismatch: " <> show arity <> " vs " <> show (length args) <> " for " <> showPretty f <> " " <> showPretty x <> " " <> showPretty f'
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
invokeStaticVars (Local' (Unique' fn)) fType = _

generateLitInstructions :: (Monad m) => Literal -> ClassBuilderT m [Instruction]
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

generatePrimInstructions :: (Monad m) => Text -> ClassBuilderT m [Instruction]
generatePrimInstructions "println" =
    pure
        [ ALoad0
        , InvokeStatic (ClassInfoType "elara.IO") "println" (MethodDescriptor [ObjectFieldType "java.lang.String"] (TypeReturn (ObjectFieldType "elara.IO")))
        ]
generatePrimInstructions "toString" =
    pure
        [ ALoad0
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
        , ALoad0
        , InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , ALoad1
        , InvokeInterface (ClassInfoType "elara.Func") "run" (MethodDescriptor [ObjectFieldType "java.lang.Object"] (TypeReturn (ObjectFieldType "java.lang.Object")))
        , CheckCast (ClassInfoType "java.lang.Integer")
        ]
generatePrimInstructions other = error $ "Unknown elara primitive: " <> showPretty other
