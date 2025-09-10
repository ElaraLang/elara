{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Compatibility instances for Effectful effects.
Particularly useful with the optics functions that operate on @Monad[Reader/State/Writer]@
-}
module Effectful.State.Extra where

import Effectful
import Effectful.State.Static.Local as P (State, get, put)
import Optics (A_Getter)

use' :: (Is k A_Getter, State a :> r) => Optic' k is a b -> Eff r b
use' lens = view lens <$> P.get

locally :: State s :> r => (s -> s) -> Eff r a -> Eff r a
locally f comp = do
    old <- P.get
    P.put $! f old
    res <- inject comp
    P.put old
    pure res

-- | Makes a 'State' effect scoped, so that any changes to the state are reverted after the computation finishes.
scoped :: forall s r a. State s :> r => Eff r a -> Eff r a
scoped = locally identity
