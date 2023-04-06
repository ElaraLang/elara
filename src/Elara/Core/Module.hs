{- | A module in the core language
This is much simpler than 'Elara.AST.Module' but has some special cases more suited to compilation
such as a special case for the Main module.
-}
module Elara.Core.Module where

import Elara.AST.Name
import Elara.AST.Typed qualified as AST
import Elara.Core qualified as Core

data Module
    = -- | The main module. This is guaranteed to contain a 'main' function.
      MainModule [Declaration]
    | Module ModuleName [Declaration]
    deriving (Show)

data Declaration
    = Value (Qualified VarName) Core.CoreExpr
    deriving (Show)