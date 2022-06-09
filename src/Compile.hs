module Compile where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Module qualified as Mod
import Data.Bifunctor
import Data.Map qualified as Map
import Elara.Package qualified as Pkg
import Error.Error qualified as E
import Print
import TypeInfer.Env (emptyEnv)
import TypeInfer.Infer (inferMany)
import TypeInfer.Value (inferDef)

compile :: Pkg.Name -> Src.Module -> Either E.Error ()
compile packageName module' = do
  canonical <- canonicalize packageName module'
  typeCheck module' canonical
  return ()

canonicalize :: Pkg.Name -> Src.Module -> Either E.Error Can.Module
canonicalize pkg = Mod.canonicalize pkg Map.empty

typeCheck :: Src.Module -> Can.Module -> Either E.Error ()
typeCheck _ canonical = do
  defs <- first E.TypeError $ inferMany (inferDef <$> canonical._decls) emptyEnv
  debugColored defs
  return ()