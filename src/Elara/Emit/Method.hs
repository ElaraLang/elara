{-# LANGUAGE OverloadedLists #-}

module Elara.Emit.Method where

import Data.List (maximum)
import {-# SOURCE #-} Elara.Emit.Expr
import Elara.Emit.State
import Elara.Emit.Var (JVMBinder (..), JVMExpr, JVMLocalType (..), toJVMExpr)
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), ReturnDescriptor (..))
import JVM.Data.Abstract.Instruction

import Data.List.NonEmpty qualified as NE
import Elara.Core (CoreExpr, Expr (..), Type, functionTypeArgs)
import Elara.Core.Analysis (declaredLambdaArity, estimateArity)
import Elara.Data.Unique
import Elara.Emit.Error
import Elara.Emit.Method.Descriptor
import Elara.Emit.Params
import Elara.Logging
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type (fieldTypeToClassInfoType)
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State (runState)
import Print
import Prelude hiding (asks)

{- | Create a method in the current class, with the given name, descriptor, and body
This handles the calculation of messiness like max stack and locals
-}
createMethod ::
    ( HasCallStack
    , Member ClassBuilder r
    , Member (Reader GenParams) r
    , Member StructuredDebug r
    , Member UniqueGen r
    , Member (Error EmitError) r
    ) =>
    QualifiedClassName ->
    NamedMethodDescriptor ->
    Text ->
    JVMExpr ->
    Sem r ()
createMethod thisClassName descriptor@(NamedMethodDescriptor args _) name body = debugWith
    ( "Creating method "
        <> showPretty thisClassName
        <> "."
        <> showPretty name
        <> " with descriptor "
        <> showPretty descriptor
        <> " and body "
        <> showPretty body
    )
    $ do
        let initialState = createMethodCreationState args thisClassName
        ((mcState, _), codeAttrs, instructions) <-
            runCodeBuilder $
                runState initialState $
                    generateInstructions body
        createMethodWith (toMethodDescriptor descriptor) [MPublic, MStatic] name codeAttrs mcState instructions

createMethodWithCodeBuilder ::
    ( Member ClassBuilder r
    , Member StructuredDebug r
    , Member (Reader GenParams) r
    ) =>
    QualifiedClassName ->
    NamedMethodDescriptor ->
    _ ->
    Text ->
    Sem (CodeBuilder : r) () ->
    Sem r ()
createMethodWithCodeBuilder thisClassName descriptor@(NamedMethodDescriptor args _) methodAttrs name codeBuilder = debugWith ("Creating method " <> showPretty thisClassName <> "." <> showPretty name <> " with descriptor " <> showPretty descriptor) $ do
    let initialState = createMethodCreationState args thisClassName
    (_, codeAttrs, instructions) <-
        subsume_ $
            runCodeBuilder codeBuilder
    createMethodWith (toMethodDescriptor descriptor) methodAttrs name codeAttrs initialState instructions

createMethodWith ::
    (Member ClassBuilder r, Member (Reader GenParams) r) =>
    MethodDescriptor ->
    _ ->
    Text ->
    [CodeAttribute] ->
    MethodCreationState ->
    [Instruction] ->
    Sem r ()
createMethodWith descriptor@(MethodDescriptor _ return_) methodAttrs name codeAttrs mcState code = do
    let maxStack = analyseMaxStack code

    let maxLocals = 1 + mcState.maxLocalVariables
    checkCasts <- asks checkCasts
    let code' =
            code <> case (return_, checkCasts) of
                (VoidReturn, _) -> [Return]
                (TypeReturn t, True) -> [CheckCast (fieldTypeToClassInfoType t), AReturn]
                (_, False) -> [AReturn]
    addMethod $
        ClassFileMethod
            methodAttrs
            name
            descriptor
            [ Code $
                CodeAttributeData
                    (fromIntegral maxStack)
                    (fromIntegral maxLocals)
                    code'
                    []
                    codeAttrs
            ]

analyseMaxStack :: [Instruction] -> Int
analyseMaxStack instructions = maximum $ scanl (+) 0 (stackChange <$> instructions)
  where
    stackChangeOf :: MethodDescriptor -> Int
    stackChangeOf (MethodDescriptor args VoidReturn) = -(length args)
    stackChangeOf (MethodDescriptor args (TypeReturn _)) = -(length args - 1)
    stackChange :: Instruction -> Int
    stackChange (InvokeDynamic _ _ desc) = stackChangeOf desc
    stackChange (InvokeStatic _ _ desc) = stackChangeOf desc
    stackChange (InvokeVirtual _ _ desc) = stackChangeOf desc
    stackChange (InvokeInterface _ _ desc) = stackChangeOf desc
    stackChange (InvokeSpecial _ _ desc) = stackChangeOf desc
    stackChange AConstNull = 1
    stackChange (ALoad _) = 1
    stackChange (AStore _) = -1
    stackChange AReturn = -1
    stackChange Dup = 1
    stackChange (CheckCast _) = 0
    stackChange (LDC _) = 1
    stackChange (GetStatic{}) = 1
    stackChange (GetField{}) = 1
    stackChange (PutStatic{}) = -1
    stackChange (PutField{}) = -2
    stackChange Return = -1
    stackChange IfEq{} = -1
    stackChange IfNe{} = -1
    stackChange IfLt{} = -1
    stackChange IfGe{} = -1
    stackChange IfGt{} = -1
    stackChange IfLe{} = -1
    stackChange Goto{} = 0
    stackChange ILoad{} = 1
    stackChange IStore{} = -1
    stackChange Label{} = 0 -- labels have no representation in the bytecode
    stackChange (New{}) = 1

etaExpandNIntoMethod ::
    ( HasCallStack
    , Member UniqueGen r
    , Member (Reader GenParams) r
    , Member StructuredDebug r
    ) =>
    CoreExpr ->
    Type ->
    QualifiedClassName ->
    NamedMethodDescriptor ->
    Sem r JVMExpr
etaExpandNIntoMethod funcCall exprType thisClassName descriptor = debugWith ("etaExpandNIntoMethod:" <> showPretty (funcCall, exprType, thisClassName)) $ do
    let arity = estimateArity funcCall - declaredLambdaArity funcCall
    debug $ "Arity: " <> showPretty arity
    let args = fmap JVMLType $ NE.take arity $ case nonEmpty $ functionTypeArgs exprType of
            Just x -> x
            Nothing -> error $ "etaExpandNIntoMethod: " <> show exprType <> " is not a function type"
    let args' = args <> drop (length args) (JVMLFieldType <$> methodDescriptorTypes descriptor)
    debug $ "etaExpandNIntoMethod: " <> showPretty (arity, args')
    params <- traverse (\_ -> makeUnique ("param" :: Text)) args'
    let paramTypes = zip params args'

    local (\x -> x{checkCasts = False}) $
        pure $
            flipfoldl'
                (\((_, pt), t) b -> App b (Var $ JVMLocal t (Just pt)))
                (toJVMExpr funcCall)
                (zip paramTypes [0 ..])
