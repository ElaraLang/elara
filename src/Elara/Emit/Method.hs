{-# LANGUAGE OverloadedLists #-}

module Elara.Emit.Method where

import Data.List (maximum)
import {-# SOURCE #-} Elara.Emit.Expr
import Elara.Emit.State
import Elara.Emit.Var (JVMBinder (..), JVMExpr, toJVMExpr)
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.Builder.Code
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), ReturnDescriptor (..))
import JVM.Data.Abstract.Instruction

import Data.List.NonEmpty qualified as NE
import Elara.Core (CoreExpr, Expr (..), Type, functionTypeArgs, functionTypeResult)
import Elara.Core.Analysis (declaredLambdaArity, estimateArity)
import Elara.Data.Unique
import Elara.Emit.Error
import Elara.Emit.Params
import Elara.Emit.Utils (generateFieldType)
import JVM.Data.Abstract.Name
import Polysemy
import Polysemy.Error
import Polysemy.Internal.Union (Union)
import Polysemy.Log (Log)
import Polysemy.Log qualified as Log
import Polysemy.Reader
import Polysemy.State (runState)
import Print

{- | Create a method in the current class, with the given name, descriptor, and body
This handles the calculation of messiness like max stack and locals
-}
createMethod ::
    ( HasCallStack
    , Member ClassBuilder r
    , Member (Reader GenParams) r
    , Member Log r
    , Member UniqueGen r
    , Member (Error EmitError) r
    ) =>
    QualifiedClassName ->
    MethodDescriptor ->
    Text ->
    JVMExpr ->
    Sem r ()
createMethod thisClassName descriptor@(MethodDescriptor args _) name body = do
    Log.debug $
        "Creating method "
            <> showPretty thisClassName
            <> "."
            <> showPretty name
            <> " with descriptor "
            <> showPretty descriptor
            <> " and body "
            <> showPretty body
    let initialState = createMethodCreationState (length args) thisClassName
    ((mcState, _), codeAttrs, instructions) <-
        runCodeBuilder $
            runState initialState $
                generateInstructions body
    createMethodWith descriptor name codeAttrs mcState instructions

createMethodWithCodeBuilder ::
    ( Member ClassBuilder r
    , Member Log r
    ) =>
    QualifiedClassName ->
    MethodDescriptor ->
    Text ->
    Sem (CodeBuilder : r) () ->
    Sem r ()
createMethodWithCodeBuilder thisClassName descriptor@(MethodDescriptor args _) name codeBuilder = do
    Log.debug $ "Creating method " <> showPretty thisClassName <> "." <> showPretty name <> " with descriptor " <> showPretty descriptor
    let initialState = createMethodCreationState (length args) thisClassName
    (_, codeAttrs, instructions) <-
        subsume_ $
            runCodeBuilder codeBuilder
    createMethodWith descriptor name codeAttrs initialState instructions

createMethodWith ::
    Member ClassBuilder r =>
    MethodDescriptor ->
    Text ->
    [CodeAttribute] ->
    MethodCreationState ->
    [Instruction] ->
    Sem r ()
createMethodWith descriptor@(MethodDescriptor _ return_) name codeAttrs mcState code = do
    let maxStack = analyseMaxStack code

    let maxLocals = 1 + mcState.maxLocalVariables
    let code' = code <> [if return_ == VoidReturn then Return else AReturn]
    addMethod $
        ClassFileMethod
            [MPublic, MStatic]
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
    stackChange Return = -1
    stackChange IfEq{} = -1
    stackChange IfNe{} = -1
    stackChange IfLt{} = -1
    stackChange IfGe{} = -1
    stackChange IfGt{} = -1
    stackChange IfLe{} = -1
    stackChange Goto{} = 0
    stackChange Label{} = 0 -- labels have no representation in the bytecode

etaExpandNIntoMethod ::
    ( HasCallStack
    , Member UniqueGen r
    , Member (Error EmitError) r
    , Member (Reader GenParams) r
    , Member Log r
    ) =>
    CoreExpr ->
    Type ->
    QualifiedClassName ->
    Sem r JVMExpr
etaExpandNIntoMethod funcCall exprType thisClassName = do
    let arity = estimateArity funcCall - declaredLambdaArity funcCall
    Log.debug $ "etaExpandNIntoMethod: " <> showPretty (funcCall, arity)
    let args = NE.take arity $ case nonEmpty $ functionTypeArgs exprType of
            Just x -> x
            Nothing -> error $ "etaExpandNIntoMethod: " <> show exprType <> " is not a function type"
    Log.debug $ "etaExpandNIntoMethod: " <> showPretty (funcCall, exprType, thisClassName, args)
    params <- traverse (\_ -> makeUnique "param") args
    let paramTypes = zip params (generateFieldType <$> args)

    local (\x -> x{checkCasts = False}) $
        pure $
            flipfoldl'
                (\(_, t) b -> App b (Var $ JVMLocal t))
                (toJVMExpr funcCall)
                (zip (fst <$> paramTypes) [0 ..])
