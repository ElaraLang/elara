{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

-- | Utility functions for creating JVM lambdas
module Elara.Emit.Lambda where

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
import JVM.Data.Abstract.Instruction (Instruction, Instruction' (..))
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.Abstract.Type (ClassInfoType (ClassInfoType), FieldType (..))
import Polysemy hiding (transform)
import Polysemy.Error
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log
import Print (showPretty)

import Data.Generics.Sum (AsAny (_As))
import Data.List (partition)
import Data.Map qualified as Map
import Data.Traversable (for)
import Elara.Core qualified as Core
import Elara.Data.Pretty
import {-# SOURCE #-} Elara.Emit.Expr (generateInstructions)
import Elara.Emit.Method.Descriptor (NamedMethodDescriptor (..), toMethodDescriptor)
import Elara.Emit.Params
import Elara.Emit.State (LVKey (..), MethodCreationState (..), addLocalVariable, createMethodCreationStateOf, lookupVar)
import Elara.Logging
import JVM.Data.Abstract.Builder.Code (CodeBuilder, emit, runCodeBuilder)
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Method
import Optics (filtered)
import Polysemy.Reader hiding (Local)
import Polysemy.State

-- | etaExpand takes a function @f@, its type @a -> b@, and generates a lambda expression @\(x : a) -> f x@
etaExpand ::
    (HasCallStack, Member ClassBuilder r, Member UniqueGen r, Member (Error EmitError) r, Member (Reader GenParams) r, Member StructuredDebug r) =>
    JVMExpr ->
    Type ->
    QualifiedClassName ->
    Sem r [Instruction]
etaExpand funcCall (stripForAll -> FuncTy i o) thisClassName = debugWith ("Eta expanding " <> showPretty funcCall <> " into \\(x : " <> showPretty i <> ") -> " <> showPretty funcCall <> " x") $ do
    param <- makeUnique "x"
    local (\x -> x{checkCasts = False}) $
        createLambda
            [(param, generateFieldType i)]
            (generateFieldType o)
            thisClassName
            ( App
                funcCall -- f
                (Var $ JVMLocal 0 (Just $ JVMLType i)) -- x
            )
etaExpand n t c = error $ "etaExpand called on non-function type: " <> showPretty (n, t, c)

{- | etaExpandN repeatedly eta expands a function until it is fully eta expanded
for example, given f : a -> b -> c, etaExpandN will return \x -> \y -> (f x) y
-}
etaExpandN ::
    (HasCallStack, Member ClassBuilder r, Member UniqueGen r, Member (Error EmitError) r, Member (Reader GenParams) r, Member StructuredDebug r) =>
    JVMExpr ->
    Type ->
    QualifiedClassName ->
    Sem r [Instruction]
etaExpandN funcCall exprType thisClassName = debugWith ("etaExpandN: " <> showPretty (funcCall, exprType, thisClassName)) $ do
    let args = case nonEmpty $ functionTypeArgs exprType of
            Just x -> x
            Nothing -> error $ "etaExpandN: " <> show exprType <> " is not a function type"
    debug $ "args: " <> showPretty args
    params <- traverse (\_ -> makeUnique "param") args
    let paramTypes = NE.zip params (generateFieldType <$> args)

    local (\x -> x{checkCasts = False}) $ do
        let body =
                flipfoldl'
                    (\((_, pt), t) b -> App b (Var $ JVMLocal t (Just $ JVMLFieldType pt)))
                    funcCall
                    (NE.zip paramTypes [0 ..])
        createLambda (toList paramTypes) (generateFieldType $ functionTypeResult exprType) thisClassName body

elaraFuncDescriptor :: (HasCallStack, IsString b, IsString a2) => FieldType -> [FieldType] -> (a2, b, MethodDescriptor, MethodDescriptor)
elaraFuncDescriptor returnType baseParams = case baseParams of
    [] -> ("Elara/Func0", "run", MethodDescriptor [] (TypeReturn (ObjectFieldType "java/lang/Object")), MethodDescriptor [] (TypeReturn returnType))
    [t] -> ("Elara/Func", "run", MethodDescriptor [ObjectFieldType "java/lang/Object"] (TypeReturn (ObjectFieldType "java/lang/Object")), MethodDescriptor [t] (TypeReturn returnType))
    [t1, t2] -> ("Elara/Func2", "run", MethodDescriptor (replicate 2 (ObjectFieldType "java/lang/Object")) (TypeReturn (ObjectFieldType "java/lang/Object")), MethodDescriptor [t1, t2] (TypeReturn returnType))
    [t1, t2, t3] -> ("Elara/Func3", "run", MethodDescriptor (replicate 3 (ObjectFieldType "java/lang/Object")) (TypeReturn (ObjectFieldType "java/lang/Object")), MethodDescriptor [t1, t2, t3] (TypeReturn returnType))
    other -> error $ "createLambda: " <> show other <> " parameters not supported"

generateLambda ::
    ( Member (Error EmitError) r
    , Member ClassBuilder r
    , Member UniqueGen r
    , Member (Reader GenParams) r
    , Member StructuredDebug r
    , Member (State MethodCreationState) r
    , Member CodeBuilder r
    ) =>
    MethodCreationState ->
    -- | The current method creation state, used to determine which variables are captured
    [(Unique Text, FieldType)] ->
    -- | The explicit parameters of the lambda - i.e. the ones in the functional interface
    FieldType ->
    -- | The return type of the lambda
    QualifiedClassName ->
    -- | The class name the lambda will be created in
    JVMExpr ->
    -- | The body of the lambda
    Sem r ()
-- generateLambda oldState explicitParams returnType thisClassName body@(Lam (Normal (Id (Local' name) t _)) b) = debugWith ("generateLambda:" <> showPretty (explicitParams, body)) $ do
--     let explicitParams' = explicitParams <> [(name, generateFieldType t)]
--     let oldState' = addLocalVariable oldState name (generateFieldType t)
--     generateLambda oldState' explicitParams' returnType thisClassName b
generateLambda oldState explicitParams returnType thisClassName body = debugWith ("generateLambda: " <> showPretty (explicitParams, body)) $ do
    (state, (_, a, b)) <- runState (createMethodCreationStateOf oldState explicitParams) $ do
        runCodeBuilder $ generateInstructions body

    -- we can now use the state to determine how which parameters are captured
    let locals = Map.toAscList state.localVariables
        locals' = fmap (\(KnownName n, (_, t)) -> (n, t)) locals
        (explicitParams', capturedParams) = partition (\(n, _) -> n `elem` (fst <$> explicitParams)) locals'
    debug $ "Captured params: " <> showPretty capturedParams
    debug $ "Explit params: " <> showPretty explicitParams
    createLambdaRaw explicitParams' capturedParams returnType thisClassName (state, a, b)

createLambdaRaw ::
    (HasCallStack, Members [ClassBuilder, CodeBuilder, Reader GenParams, UniqueGen, Error EmitError, StructuredDebug] r, Member (State MethodCreationState) r) =>
    -- | The base parameters of the lambda - i.e. ones in the functional interface
    [(Unique Text, FieldType)] ->
    -- | Extra "captured" parameters
    [(Unique Text, FieldType)] ->
    -- | The return type of the lambda
    FieldType ->
    -- | The class name the lambda will be created in
    QualifiedClassName ->
    -- | The body of the lambda
    (MethodCreationState, [CodeAttribute], [Instruction]) ->
    Sem r ()
createLambdaRaw baseParams captureParams returnType thisClassName (state, attr, bodyInsts) = do
    lamSuffix <- makeUniqueId
    let params = captureParams <> baseParams
    let lambdaMethodName = "lambda$" <> show lamSuffix
    debug $ "Creating lambda " <> showPretty lambdaMethodName <> " which captures: " <> showPretty captureParams

    let lambdaMethodDescriptor = MethodDescriptor (snd <$> toList params) (TypeReturn returnType)

    createMethodWith lambdaMethodDescriptor [MPublic, MStatic] lambdaMethodName attr state bodyInsts
    let (functionalInterface, invoke, baseMethodDescriptor, methodDescriptor) = elaraFuncDescriptor returnType (snd <$> baseParams)

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
                    [ BMMethodArg baseMethodDescriptor
                    , BMMethodHandleArg
                        ( MHInvokeStatic
                            ( MethodRef
                                (ClassInfoType thisClassName)
                                lambdaMethodName
                                lambdaMethodDescriptor
                            )
                        )
                    , BMMethodArg methodDescriptor
                    ]
                )
                invoke
                (MethodDescriptor (snd <$> captureParams) (TypeReturn $ ObjectFieldType functionalInterface))
    for_ captureParams $ \(cp, _) -> do
        (cp', _) <- lookupVar cp
        emit $ ALoad cp'
    emit inst

{- | Creates the bytecode for a lambda expression
This involves a few steps:
1. Create a method that implements the lambda's body
2. Creates a bootstrap method that calls the LambdaMetaFactory to create the lambda
3. Returns an invokedynamic instruction that calls the bootstrap method
-}
createLambda ::
    (HasCallStack, Members [ClassBuilder, Reader GenParams, UniqueGen, Error EmitError, StructuredDebug] r) =>
    -- | The names and parameters of the lambda
    [(Unique Text, FieldType)] ->
    -- | The return type of the lambda
    FieldType ->
    -- | The class name the lambda will be created in
    QualifiedClassName ->
    -- | The body of the lambda
    JVMExpr ->
    Sem r [Instruction]
createLambda baseParams returnType thisClassName body = do
    lamSuffix <- makeUniqueId
    captureParams <- getCapturedParams baseParams body
    let params = captureParams <> baseParams
    let lambdaMethodName = "lambda$" <> show lamSuffix
    debug $ "Creating lambda " <> showPretty lambdaMethodName <> " which captures: " <> showPretty captureParams

    let lambdaMethodDescriptor = NamedMethodDescriptor (toList params) (TypeReturn returnType)
    debug $
        "Creating lambda method "
            <> showPretty lambdaMethodName
            <> " with descriptor "
            <> showPretty lambdaMethodDescriptor
            <> " and body "
            <> showPretty body

    let paramOffset = fromIntegral $ length captureParams
    let offsetBody =
            transform
                ( \case
                    Core.Var (JVMLocal i t) -> Core.Var (JVMLocal (i - paramOffset) t)
                    x -> x
                )
                body
    let body' =
            foldr
                (\((n, pt), i) b -> replaceVar' (Local' n) (JVMLocal (i + paramOffset) (Just $ JVMLFieldType pt)) b)
                offsetBody
                (zip (toList params) [0 ..])

    debug $ "Body: " <> showPretty body <> " --> " <> showPretty body'

    createMethod thisClassName lambdaMethodDescriptor lambdaMethodName body'
    let (functionalInterface, invoke, baseMethodDescriptor, methodDescriptor) = elaraFuncDescriptor returnType (snd <$> baseParams)

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
                    [ BMMethodArg baseMethodDescriptor
                    , BMMethodHandleArg
                        ( MHInvokeStatic
                            ( MethodRef
                                (ClassInfoType thisClassName)
                                lambdaMethodName
                                (toMethodDescriptor lambdaMethodDescriptor)
                            )
                        )
                    , BMMethodArg methodDescriptor
                    ]
                )
                invoke
                (MethodDescriptor (snd <$> captureParams) (TypeReturn $ ObjectFieldType functionalInterface))

    debug $ "Created lambda " <> showPretty lambdaMethodName
    pure $ map (ALoad . fromIntegral) [1 .. length captureParams] <> [inst]

lambdaTypeName :: Int -> QualifiedClassName
lambdaTypeName 0 = "Elara/Func0"
lambdaTypeName 1 = "Elara/Func"
lambdaTypeName 2 = "Elara/Func2"
lambdaTypeName 3 = "Elara/Func3"
lambdaTypeName n = error $ "lambdaTypeName: " <> show n <> " not supported"

{- | Inspects a lambda expression to determine which local variables are captured, returning a generated name for each, and its corresponding type
For example, if we have \x -> local_1, we know that local_1 must be captured from an outer scope (as it is not an argument to the lambda)
-}
getCapturedParams :: Member UniqueGen r => [(Unique Text, FieldType)] -> JVMExpr -> Sem r [(Unique Text, FieldType)]
getCapturedParams params expr = do
    let len = fromIntegral $ length params
    -- Get all locals_<n> where n >= length params
    let locals =
            expr
                ^.. cosmos
                % _As @"Var"
                % _As @"JVMLocal"
                % filtered (\(a, _) -> a >= len)

    let locals' = locals ^.. traversed % _2 % _Just
    for locals' $ \t -> do
        n <- makeUnique "local"
        let t' = case t of
                JVMLFieldType ft -> ft
                JVMLType t -> generateFieldType t
        pure (n, t')
