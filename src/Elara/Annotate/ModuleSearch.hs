{-# LANGUAGE FunctionalDependencies #-}

module Elara.Annotate.ModuleSearch where

import Data.Map qualified as M
import Elara.AST.Module (Module)
import Elara.AST.Name (ModuleName)
import Elara.AST.Select (Frontend)
