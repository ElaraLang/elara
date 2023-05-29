{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Compatibility instances for polysemy effects.
Particularly useful with the lens functions that operate on @Monad[Reader/State/Writer]@
-}
module Polysemy.MTL where

import Control.Lens (Lens', set, view)
import Control.Monad.State.Class
import Polysemy
import Polysemy.State as P (State (Get, Put), get, gets, modify', put)

instance MonadState s (Sem (State s ': r)) where
    get = P.get
    put = P.put

stateToStateViaLens ::
    Member (State bigSt) r =>
    Lens' bigSt smallSt ->
    Sem (State smallSt ': r) a ->
    Sem r a
stateToStateViaLens lens = interpret $ \case
    Put smallSt -> P.modify' (set lens smallSt)
    Get -> P.gets (view lens)
