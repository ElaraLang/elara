module HedgehogSyd where

import Hedgehog
import Test.Syd

instance MonadTest (TestDefM outers inners) where
    liftTest test = liftTest $ liftTest test
