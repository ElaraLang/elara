module Compile where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Module qualified as Mod
import Data.Map qualified as Map
import Elara.Name (Name)
import Error.Error qualified as E

compile :: Name -> Src.Module -> Either E.Error ()
compile packageName module' = do
  canonical <- canonicalize packageName module'
  types <- typeCheck module' canonical
  return ()

canonicalize :: Name -> Src.Module -> Either E.Error Can.Module
canonicalize pkg module' = Mod.canonicalize pkg Map.empty module'

typeCheck :: Src.Module -> Can.Module -> Either E.Error ()
typeCheck module' canonical = undefined
