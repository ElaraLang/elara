{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Compatibility instances for polysemy effects.
Particularly useful with the lens functions that operate on Monad[Reader/State/Writer]
-}
module Polysemy.MTL where

import Control.Monad.State.Class as MTLS
import Polysemy
import Polysemy.State as P
import Prelude hiding (State, get, put, state)

instance MonadState s (Sem (State s ': r)) where
    get = P.get
    put = P.put
