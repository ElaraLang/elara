module Compile where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Module qualified as Mod
import Data.Bifunctor
import Data.Map qualified as Map
import Elara.Name (Name)
import Elara.Package qualified as Pkg
import Error.Error qualified as E
import TypeInfer.Env (emptyEnv)
import TypeInfer.Infer (inferMany, runInfer)
import TypeInfer.Value (inferDef)
import Debug.Trace

compile :: Pkg.Name -> Src.Module -> Either E.Error ()
compile packageName module' = do
  canonical <- canonicalize packageName module'
  types <- typeCheck module' canonical
  return ()

canonicalize :: Pkg.Name -> Src.Module -> Either E.Error Can.Module
canonicalize pkg module' = Mod.canonicalize pkg Map.empty module'

typeCheck :: Src.Module -> Can.Module -> Either E.Error ()
typeCheck module' canonical = do
  traceShowM canonical
  defs <- first E.TypeError $ inferMany (inferDef <$> canonical._decls) emptyEnv
  traceShowM defs
  return ()