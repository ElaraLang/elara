-- | Utility functions for creating JVM lambdas
module Elara.Emit.Lambda where

import Data.Hashable (hash)
import Data.Map qualified as M
import Elara.Core
import Elara.Data.Unique
import Elara.Emit.Method
import Elara.Emit.Var
import JVM.Data.Abstract.Builder (ClassBuilder, addMethod)
import JVM.Data.Abstract.ClassFile.Method (ClassFileMethod (ClassFileMethod))
import JVM.Data.Abstract.ConstantPool (BootstrapArgument (BMMethodArg, BMMethodHandleArg), BootstrapMethod (BootstrapMethod), MethodHandleEntry (..), MethodRef (MethodRef))
import JVM.Data.Abstract.Descriptor (MethodDescriptor (MethodDescriptor), ReturnDescriptor (..))
import JVM.Data.Abstract.Instruction (Instruction, Instruction' (InvokeDynamic))
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.Abstract.Type (ClassInfoType (ClassInfoType), FieldType (..))
import Polysemy

functionalInterfaces :: Map ([FieldType], Maybe FieldType) (QualifiedClassName, Text, MethodDescriptor)
functionalInterfaces =
    fromList
        [ --  (([], Nothing), ("ElaraRunnable", "run"))
          -- , (([ClassInfoType "T"], Nothing), ("ElaraSupplier", "get"))
          (([ObjectFieldType "java/lang/Object"], Just (ObjectFieldType "java/lang/Object")), ("elara/Func", "run", MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn $ ObjectFieldType "java/lang/Object")))
        ]

{- | Creates the bytecode for a lambda expression
This involves a few steps:
1. Create a method that implements the lambda's body
2. Creates a bootstrap method that calls the LambdaMetaFactory to create the lambda
3. Returns an invokedynamic instruction that calls the bootstrap method
-}
createLambda :: Member ClassBuilder r => [(Unique Text, FieldType)] -> FieldType -> QualifiedClassName -> JVMExpr -> Sem r Instruction
createLambda params returnType thisClassName body = do
    let lambdaMethodName = "lambda$" <> show (hash body)
    let lambdaDescriptor = (params, returnType)
    let lambdaMethodDescriptor = MethodDescriptor (snd <$> fst lambdaDescriptor) (TypeReturn $ snd lambdaDescriptor)

    createMethod thisClassName lambdaMethodDescriptor lambdaMethodName body
    let (functionalInterface, invoke, methodDescriptor) =
            fromMaybe (error "No functional interface for lambda") $
                M.lookup (map snd params, Just returnType) functionalInterfaces

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
    pure inst
