{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Maybe where

import Polysemy
import Polysemy.Error (runError, throw)

-- | A 'Maybe' effect, which can be used to short-circuit computations without a dedicated error type.
data MaybeE m a where
    JustE :: a -> MaybeE m a
    NothingE :: MaybeE m a

makeSem ''MaybeE

runMaybe :: Sem (MaybeE : r1) r2 -> Sem r1 (Maybe r2)
runMaybe =
    fmap rightToMaybe
        . runError
        . reinterpret
            ( \case
                JustE a -> pure a
                NothingE -> throw ()
            )
