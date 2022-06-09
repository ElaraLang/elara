module TypeInfer.Pattern where

import AST.Canonical qualified as Can
import Control.Monad.RWS.Strict (gets)
import Elara.Name qualified as Name
import Print (debugColored)
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as E
import TypeInfer.Type qualified as E
import TypeInfer.Type qualified as T

inferPattern :: Can.Pattern -> Infer T.Type
inferPattern (Can.PVar name) = do
  tv <- E.freshTVar
  env <- gets E.typeEnv
  let sc = E.Forall [] tv
  E.addToEnv (Name.value name, sc)
  return tv
