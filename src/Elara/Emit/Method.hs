module Elara.Emit.Method where

import Data.List
import Elara.Emit.Expr
import Elara.Emit.State
import Elara.Emit.Var (JVMExpr)
import JVM.Data.Abstract.Builder (ClassBuilderT, addMethod)
import JVM.Data.Abstract.Builder.Code (runCodeBuilder, runCodeBuilder')
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), ReturnDescriptor (..))
import JVM.Data.Abstract.Instruction
import JVM.Data.Convert.Instruction (fullyRunCodeConverter)
import Polysemy (runM)
import Polysemy.State (runState)

{- | Create a method in the current class, with the given name, descriptor, and body
This handles the calculation of messiness like max stack and locals
-}
createMethod :: Monad m => MethodDescriptor -> Text -> JVMExpr -> ClassBuilderT m ()
createMethod descriptor@(MethodDescriptor args _) name body = do
    let initialState = createMethodCreationState (length args)
    let ((mcState, _), instructions) =
            runCodeBuilder' $
                runM $
                    runState initialState $
                        generateInstructions body
    createMethodWith descriptor name mcState instructions

createMethodWith :: Monad m => MethodDescriptor -> Text -> MethodCreationState -> [Instruction] -> ClassBuilderT m ()
createMethodWith descriptor@(MethodDescriptor _ return_) name mcState code = do
    let maxStack = analyseMaxStack code
    let maxLocals = 1 + mcState.maxLocalVariables

    addMethod $
        ClassFileMethod
            [MPublic, MStatic]
            name
            descriptor
            [ Code $
                CodeAttributeData
                    (fromIntegral maxStack)
                    (fromIntegral maxLocals)
                    (code <> [if return_ == VoidReturn then Return else AReturn])
                    []
                    []
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
    stackChange ALoad0 = 1
    stackChange ALoad1 = 1
    stackChange ALoad2 = 1
    stackChange ALoad3 = 1
    stackChange (ALoad _) = 1
    stackChange (AStore _) = -1
    stackChange AStore0 = -1
    stackChange AStore1 = -1
    stackChange AStore2 = -1
    stackChange AStore3 = -1
    stackChange AReturn = -1
    stackChange AThrow = -1
    stackChange (CheckCast _) = 0
    stackChange (LDC _) = 1
    stackChange (GetStatic{}) = 1
    stackChange (PutStatic{}) = -1
    stackChange Return = -1
