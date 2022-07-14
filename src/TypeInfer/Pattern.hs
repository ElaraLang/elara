module TypeInfer.Pattern where

import AST.Canonical qualified as Can
import Elara.Name qualified as Name
import Print (debugColoredStr)
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as E
import TypeInfer.Type qualified as E
import TypeInfer.Type qualified as T

inferPattern :: Can.Pattern -> Infer T.Type
inferPattern (Can.PVar name) = do
  tv <- E.freshTVar
  let sc = E.Forall [] tv -- Type variables themselves are *not* polytypes
  E.addToEnv (Name.value name, sc)
  debugColoredStr $ "Inferring pattern " ++ show name ++ ": " ++ show sc
  return tv
inferPattern Can.PWildcard = E.freshTVar
inferPattern other = error $ "Pattern.inferPattern: " ++ show other
