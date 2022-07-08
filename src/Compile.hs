module Compile where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Module qualified as Mod
import Data.Bifunctor
import Data.Map qualified as M
import Data.Map qualified as Map
import Elara.Package qualified as Pkg
import Error.Error qualified as E
import Pretty (prettyPrint)
import Print
import TypeInfer.Env (TypeEnv (TypeEnv), emptyEnv)
import TypeInfer.Infer (infer, inferMany, runInfer)
import TypeInfer.Module (inferModule)
import TypeInfer.Type
import TypeInfer.Value (inferDef)

compile :: Pkg.Name -> Src.Module -> Either E.Error ()
compile packageName module' = do
  canonical <- canonicalize packageName module'
  debugColored canonical
  typeCheck module' canonical
  return ()

canonicalize :: Pkg.Name -> Src.Module -> Either E.Error Can.Module
canonicalize pkg = Mod.canonicalize pkg Map.empty

typeCheck :: Src.Module -> Can.Module -> Either E.Error ()
typeCheck _ canonical = do
  let emptyEnv =
        let int = TCon "Int"
         in TypeEnv $
              M.fromList
                [ ("*", Forall [] (TFunc int (TFunc int int))),
                  ("-", Forall [] (TFunc int (TFunc int int)))
                ]
  ((TypeEnv defs), _) <- first E.TypeError $ inferMany (inferModule canonical) emptyEnv
  debugColored . prettyPrint $ defs
  return ()