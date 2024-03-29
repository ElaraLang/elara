{-# LANGUAGE ViewPatterns #-}

-- | Utility functions for creating JVM lambdas
module Elara.Emit.Lambda where

import Data.Hashable (hash)
import Elara.AST.VarRef
import Elara.Core
import Elara.Data.Unique
import Elara.Emit.Error (EmitError)
import Elara.Emit.Method
import Elara.Emit.Utils (generateFieldType)
import Elara.Emit.Var
import Elara.ToCore
import JVM.Data.Abstract.Builder (ClassBuilder)
import JVM.Data.Abstract.ConstantPool (BootstrapArgument (BMMethodArg, BMMethodHandleArg), BootstrapMethod (BootstrapMethod), MethodHandleEntry (..), MethodRef (MethodRef))
import JVM.Data.Abstract.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (..))
import JVM.Data.Abstract.Instruction (Instruction, Instruction' (InvokeDynamic))
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.Abstract.Type (ClassInfoType (ClassInfoType), FieldType (..))
import Polysemy
import Polysemy.Error
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log
import Print (showPretty)

import Elara.Emit.Params
import Polysemy.Reader

functionalInterfaces :: Map ([FieldType], Maybe FieldType) (QualifiedClassName, Text, MethodDescriptor)
functionalInterfaces =
    fromList
        [ --  (([], Nothing), ("ElaraRunnable", "run"))
          -- , (([ClassInfoType "T"], Nothing), ("ElaraSupplier", "get"))
          (([ObjectFieldType "java/lang/Object"], Just (ObjectFieldType "java/lang/Object")), ("elara/Func", "run", MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn $ ObjectFieldType "java/lang/Object")))
        ]

-- | etaExpand takes a function @f@, its type @a -> b@, and generates a lambda expression @\(x : a) -> f x@
etaExpand :: (Member ClassBuilder r, Member UniqueGen r, Member (Error EmitError) r, Member (Reader GenParams) r, Member Log r) => JVMExpr -> Type -> QualifiedClassName -> Sem r Instruction
etaExpand funcCall (stripForAll -> FuncTy i o) thisClassName = do
    Log.debug $ "Eta expanding " <> showPretty funcCall <> " into \\(x : " <> showPretty i <> ") -> " <> showPretty funcCall <> " x"
    param <- makeUnique "x"
    local (\x -> x{checkCasts = False}) $
        createLambda
            (param, generateFieldType i)
            (generateFieldType o)
            thisClassName
            ( App
                funcCall -- f
                (Var $ JVMLocal 0) -- x
            )
etaExpand n t c = error $ "etaExpand called on non-function type: " <> showPretty (n, t, c)

{- | Creates the bytecode for a lambda expression
This involves a few steps:
1. Create a method that implements the lambda's body
2. Creates a bootstrap method that calls the LambdaMetaFactory to create the lambda
3. Returns an invokedynamic instruction that calls the bootstrap method
-}
createLambda :: (Member ClassBuilder r, Member (Reader GenParams) r, Member UniqueGen r, Member (Error EmitError) r, Member Log r) => (Unique Text, FieldType) -> FieldType -> QualifiedClassName -> JVMExpr -> Sem r Instruction
createLambda params returnType thisClassName body = do
    let lambdaMethodName = "lambda$" <> show (abs $ hash body)

    let lambdaMethodDescriptor = MethodDescriptor [snd params] (TypeReturn returnType)
    Log.debug $ "Creating lambda method " <> showPretty lambdaMethodName <> " with descriptor " <> showPretty lambdaMethodDescriptor

    let body' = replaceVar' (Local' $ fst params) (JVMLocal 0) body

    createMethod thisClassName lambdaMethodDescriptor lambdaMethodName body'
    let (functionalInterface, invoke, methodDescriptor) =
            ("elara/Func", "run", MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn $ ObjectFieldType "java/lang/Object"))

    let inst =
            InvokeDynamic
                ( BootstrapMethod
                    ( MHInvokeStatic
                        ( MethodRef
                            (ClassInfoType "java/lang/invoke/LambdaMetafactory")
                            "metafactory"
                            ( MethodDescriptor
                                [ ObjectFieldType "java/lang/invoke/MethodHandles$Lookup"
                                , ObjectFieldType "java/lang/String"
                                , ObjectFieldType "java/lang/invoke/MethodType"
                                , ObjectFieldType "java/lang/invoke/MethodType"
                                , ObjectFieldType "java/lang/invoke/MethodHandle"
                                , ObjectFieldType "java/lang/invoke/MethodType"
                                ]
                                (TypeReturn $ ObjectFieldType "java/lang/invoke/CallSite")
                            )
                        )
                    )
                    [ BMMethodArg methodDescriptor
                    , BMMethodHandleArg
                        ( MHInvokeStatic
                            ( MethodRef
                                (ClassInfoType thisClassName)
                                lambdaMethodName
                                lambdaMethodDescriptor
                            )
                        )
                    , BMMethodArg lambdaMethodDescriptor
                    ]
                )
                invoke
                (MethodDescriptor [] (TypeReturn $ ObjectFieldType functionalInterface))

    Log.debug $ "Created lambda " <> showPretty lambdaMethodName
    pure inst
