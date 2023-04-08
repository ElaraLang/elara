{-# LANGUAGE TemplateHaskell #-}

{- | A module in the core language
This is much simpler than 'Elara.AST.Module' but has some special cases more suited to compilation
such as a special case for the Main module.
-}
module Elara.Core.Module where

import Control.Lens
import Elara.AST.Name
import Elara.Core qualified as Core
import Elara.Data.Pretty (Pretty (pretty))

data Module
  = -- | The main module. This is guaranteed to contain a 'main' function.
    MainModule ModuleName [Declaration]
  | Module ModuleName [Declaration]
  deriving (Show)

moduleName :: Lens' Module ModuleName
moduleName = lens getter setter
 where
  getter (MainModule n _) = n
  getter (Module n _) = n
  setter (MainModule _ d) n = MainModule n d
  setter (Module _ d) n = Module n d

data Declaration
  = Value (Qualified VarName) Core.CoreExpr
  deriving (Show)

makePrisms ''Module
makePrisms ''Declaration

instance Pretty Module where
  pretty (MainModule n d) = pretty n <> " (Main)" <> pretty d
  pretty (Module n d) = pretty n <> pretty d

instance Pretty Declaration where
  pretty (Value n e) = pretty n <> " = " <> pretty e