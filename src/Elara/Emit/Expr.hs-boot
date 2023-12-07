module Elara.Emit.Expr where

import Elara.Core
import Elara.Emit.State
import Elara.Emit.Var
import JVM.Data.Abstract.Builder.Code
import Polysemy
import Polysemy.State
import JVM.Data.Abstract.Builder

generateInstructions ::
    ( HasCallStack
    , Member (State MethodCreationState) r
    , Member (CodeBuilder) r
    , Member (ClassBuilder) r
    ) =>
    Expr JVMBinder ->
    Sem r ()