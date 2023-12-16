module Elara.Emit.Expr where

import Elara.Core
import Elara.Data.Unique
import Elara.Emit.Error
import Elara.Emit.State
import Elara.Emit.Var
import JVM.Data.Abstract.Builder
import JVM.Data.Abstract.Builder.Code
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Reader
import Elara.Emit.Params

generateInstructions ::
    ( HasCallStack
    , Member (State MethodCreationState) r
    , Member CodeBuilder r
    , Member ClassBuilder r
    , Member UniqueGen r
    , Member (Error EmitError) r
    , Member (Reader GenParams) r
    ) =>
    Expr JVMBinder ->
    Sem r ()