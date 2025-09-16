module Effectful.Error.Extra where

import Effectful
import Effectful.Error.Static qualified as Eff

fromEither :: (Eff.Error e :> r, Show e) => Either e a -> Eff r a
fromEither = either Eff.throwError pure
