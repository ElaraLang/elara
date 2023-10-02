module Elara.Emit.Expr where

import Elara.Core
import Elara.Emit.Var
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.Instruction
import Print (showPretty)

generateInstructions :: (Monad m) => Expr JVMBinder -> ClassBuilderT m [Instruction]
generateInstructions (Var (JVMLocal 0)) = pure [ALoad0]
generateInstructions other = error $ "Not implemented: " <> showPretty other
