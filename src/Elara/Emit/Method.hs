module Elara.Emit.Method where

import Data.List
import Elara.AST.Name
import Elara.AST.VarRef (UnlocatedVarRef, VarRef' (..))
import Elara.Core
import Elara.Data.Unique
import Elara.Emit.Expr
import Elara.Emit.Var (JVMBinder (..), replaceVar, replaceVar', toJVMExpr)
import JVM.Data.Abstract.Builder (ClassBuilder, ClassBuilderT, addMethod)
import JVM.Data.Abstract.ClassFile.Method
import JVM.Data.Abstract.Descriptor (MethodDescriptor (..), ReturnDescriptor (TypeReturn))
import JVM.Data.Abstract.Instruction
import JVM.Data.Abstract.Type

-- | All the information we need to create a MethodDescriptor, plus parameter names
type NamedMethodDescriptor = ([(Unique Text, FieldType)], FieldType)

transformMethodParameters :: [UnlocatedVarRef Text] -> CoreExpr -> Expr JVMBinder
transformMethodParameters params body =
    let jvm = toJVMExpr body in foldr (\(name, idx) e -> replaceVar' (name) (JVMLocal idx) e) jvm (zip (params) [0 ..])

{- | Create a method in the current class, with the given name, descriptor, and body
This handles the calculation of messiness like max stack and locals
-}
createMethod :: (Monad m) => NamedMethodDescriptor -> Text -> CoreExpr -> ClassBuilderT m ()
createMethod descriptor name body = do
    let body' = transformMethodParameters ((Local . Identity . fst) <$> fst descriptor) body
    code <- generateInstructions body'
    let maxStack = analyseMaxStack code

    addMethod $
        ClassFileMethod
            []
            name
            (MethodDescriptor (snd <$> fst descriptor) (TypeReturn $ snd descriptor))
            [ Code $
                CodeAttributeData
                    maxStack
                    (2 {- TODO -})
                    code
                    []
                    []
            ]

analyseMaxStack :: [Instruction] -> Int
analyseMaxStack instructions = maximum $ scanl (+) 0 (stackChange <$> instructions)
  where
    stackChange :: Instruction -> Int
    stackChange (InvokeDynamic{}) = 1
    stackChange (InvokeStatic{}) = 1
    stackChange (InvokeVirtual{}) = 1
    stackChange (AConstNull) = 1
    stackChange (ALoad0) = 1
    stackChange (AReturn) = -1
    stackChange (AThrow) = -1
    stackChange (CheckCast _) = 0
    stackChange (LDC _) = 1
    stackChange (GetStatic{}) = 1
    stackChange (PutStatic{}) = -1
    stackChange Return = -1
