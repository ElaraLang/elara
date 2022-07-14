module TypeInfer.Module where

import AST.Canonical qualified as Src
import Control.Monad (when)
import Control.Monad.RWS (gets)
import Data.Maybe (isNothing)
import Elara.Name qualified as Name
import Error.Error qualified as E
import TypeInfer.Env (Infer)
import TypeInfer.Env qualified as E
import TypeInfer.Type qualified as E
import TypeInfer.Value qualified as I

inferModule :: Src.Module -> Infer [E.Type]
inferModule mod' = do
  addGenericBindings (mod'._decls)
  mapM I.inferDef mod'._decls

addGenericBindings :: [Src.Def] -> Infer ()
addGenericBindings = mapM_ addGenericBinding

addGenericBinding :: Src.Def -> Infer ()
addGenericBinding def = do
  tv <- E.freshTVar
  let scheme = E.Forall [] tv
  let name = Name.value $ Src.defName def
  inEnv <- E.maybeLookupEnv name
  when (isNothing inEnv) $ do
    -- Don't override the def if there is one
    E.addToEnv (name, scheme)
