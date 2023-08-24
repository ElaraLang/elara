{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Compatibility instances for polysemy effects.
Particularly useful with the lens functions that operate on @Monad[Reader/State/Writer]@
-}
module Polysemy.State.Extra where

import Control.Lens (Getting, Lens', set, view)
import Polysemy
import Polysemy.State as P (State (Get, Put), get, gets, modify', put)

use' :: (Member (State s) r) => Getting a s a -> Sem r a
use' lens = view lens <$> P.get

locally :: (Member (State s) r) => (s -> s) -> Sem r a -> Sem r a
locally f comp = do
    old <- P.get
    P.put $! f old
    res <- raise_ comp
    P.put old
    pure res

-- | Makes a 'State' effect scoped, so that any changes to the state are reverted after the computation finishes.
scoped :: forall s r a. (Member (State s) r) => Sem r a -> Sem r a
scoped = locally identity

stateToStateViaLens ::
    (Member (State bigSt) r) =>
    Lens' bigSt smallSt ->
    Sem (State smallSt ': r) a ->
    Sem r a
stateToStateViaLens lens = interpret $ \case
    Put smallSt -> P.modify' (set lens smallSt)
    Get -> P.gets (view lens)
