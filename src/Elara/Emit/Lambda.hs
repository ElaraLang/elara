{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

-- | Utility functions for creating JVM lambdas
module Elara.Emit.Lambda where

import Data.Hashable (hash)
import Data.List.NonEmpty qualified as NE
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

-- | etaExpand takes a function @f@, its type @a -> b@, and generates a lambda expression @\(x : a) -> f x@
etaExpand ::
    (HasCallStack, Member ClassBuilder r, Member UniqueGen r, Member (Error EmitError) r, Member (Reader GenParams) r, Member Log r) =>
    JVMExpr ->
    Type ->
    QualifiedClassName ->
    Sem r Instruction
etaExpand funcCall (stripForAll -> FuncTy i o) thisClassName = do
    Log.debug $ "Eta expanding " <> showPretty funcCall <> " into \\(x : " <> showPretty i <> ") -> " <> showPretty funcCall <> " x"
    param <- makeUnique "x"
    local (\x -> x{checkCasts = False}) $
        createLambda
            ((param, generateFieldType i) :| [])
            (generateFieldType o)
            thisClassName
            ( App
                funcCall -- f
                (Var $ JVMLocal 0) -- x
            )
etaExpand n t c = error $ "etaExpand called on non-function type: " <> showPretty (n, t, c)

{- | etaExpandN repeatedly eta expands a function until it is fully eta expanded
for example, given f : a -> b -> c, etaExpandN will return \x -> \y -> (f x) y
-}
etaExpandN ::
    (HasCallStack, Member ClassBuilder r, Member UniqueGen r, Member (Error EmitError) r, Member (Reader GenParams) r, Member Log r) =>
    JVMExpr ->
    Type ->
    QualifiedClassName ->
    Sem r Instruction
etaExpandN funcCall exprType thisClassName = do
    let args = case nonEmpty $ functionTypeArgs exprType of
            Just x -> x
            Nothing -> error $ "etaExpandN: " <> show exprType <> " is not a function type"
    Log.debug $ "etaExpandN: " <> showPretty (funcCall, exprType, thisClassName, args)
    params <- traverse (\_ -> makeUnique "param") args
    let paramTypes = NE.zip params (generateFieldType <$> args)

    local (\x -> x{checkCasts = False}) $
        createLambda paramTypes (generateFieldType $ functionTypeResult exprType) thisClassName $
            flipfoldl'
                (\(_, t) b -> App b (Var $ JVMLocal t))
                funcCall
                (NE.zip (fst <$> paramTypes) [0 ..])

{- | Creates the bytecode for a lambda expression
This involves a few steps:
1. Create a method that implements the lambda's body
2. Creates a bootstrap method that calls the LambdaMetaFactory to create the lambda
3. Returns an invokedynamic instruction that calls the bootstrap method
-}
createLambda ::
    (HasCallStack, Members [ClassBuilder, Reader GenParams, UniqueGen, Error EmitError, Log] r) =>
    -- | The names and parameters of the lambda
    NonEmpty (Unique Text, FieldType) ->
    -- | The return type of the lambda
    FieldType ->
    -- | The class name the lambda will be created in
    QualifiedClassName ->
    -- | The body of the lambda
    JVMExpr ->
    Sem r Instruction
createLambda params returnType thisClassName body = do
    lamSuffix <- makeUniqueId
    let lambdaMethodName = "lambda$" <> show lamSuffix

    let lambdaMethodDescriptor = MethodDescriptor (toList $ snd <$> params) (TypeReturn returnType)
    Log.debug $
        "Creating lambda method "
            <> showPretty lambdaMethodName
            <> " with descriptor "
            <> showPretty lambdaMethodDescriptor
            <> " and body "
            <> showPretty body

    let body' = foldr (\(n, i) b -> replaceVar' (Local' n) (JVMLocal i) b) body (zip (fst <$> toList params) [0 ..])

    createMethod thisClassName lambdaMethodDescriptor lambdaMethodName body'
    let (functionalInterface, invoke, methodDescriptor) =
            case length params of
                1 -> ("Elara/Func", "run", MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn $ ObjectFieldType "java/lang/Object"))
                2 -> ("Elara/Func2", "run", MethodDescriptor [ObjectFieldType "java/lang/Object", ObjectFieldType "java/lang/Object"] (TypeReturn $ ObjectFieldType "java/lang/Object"))
                3 -> ("Elara/Func3", "run", MethodDescriptor [ObjectFieldType "java/lang/Object", ObjectFieldType "java/lang/Object", ObjectFieldType "java/lang/Object"] (TypeReturn $ ObjectFieldType "java/lang/Object"))
                other -> error $ "createLambda: " <> show other <> " parameters not supported"

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
