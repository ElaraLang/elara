module Elara.Emit.Method where

import Data.List ( maximum )
import Elara.Emit.Expr
import Elara.Emit.State
import Elara.Emit.Var (JVMExpr)
import JVM.Data.Abstract.Builder (ClassBuilderT, addMethod)
import JVM.Data.Abstract.Builder.Code (runCodeBuilder')
import JVM.Data.Abstract.ClassFile.AccessFlags
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), ReturnDescriptor (..))
import JVM.Data.Abstract.Instruction
import JVM.Data.Analyse.Instruction (calculateStackMapFrames)
import Polysemy (runM)
import Polysemy.State (runState)

{- | Create a method in the current class, with the given name, descriptor, and body
This handles the calculation of messiness like max stack and locals
-}
createMethod :: Monad m => MethodDescriptor -> Text -> JVMExpr -> ClassBuilderT m ()
createMethod descriptor@(MethodDescriptor args _) name body = do
    let initialState = createMethodCreationState (length args)
    let ((mcState, _), codeAttrs, instructions) =
            runCodeBuilder' $
                runM $
                    runState initialState $
                        generateInstructions body
    createMethodWith descriptor name codeAttrs mcState instructions

createMethodWith :: Monad m => MethodDescriptor -> Text -> [CodeAttribute] -> MethodCreationState -> [Instruction] -> ClassBuilderT m ()
createMethodWith descriptor@(MethodDescriptor _ return_) name codeAttrs mcState code = do
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
                    (StackMapTable (calculateStackMapFrames descriptor code) : codeAttrs)
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
    stackChange (CheckCast _) = 0
    stackChange (LDC _) = 1
    stackChange (GetStatic{}) = 1
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
