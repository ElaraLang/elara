-- | Emits JVM bytecode from Elara AST.
module Elara.Emit where

import Elara.AST.Module (Module)
import Elara.AST.Select (Typed)
import Elara.Data.TopologicalGraph (TopologicalGraph)
import JVM.Data.JVMVersion

emitGraph :: TopologicalGraph (Module Typed)
emitGraph = undefined
