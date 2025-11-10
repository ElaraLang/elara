module Elara.Query.Common where

import qualified Elara.Query
import qualified Rock

{- | Appends 'Rock' to a list of effects.
  This type mainly exists to avoid a cyclic import between this module and 'Elara.Query.Effects'
-}
type WithRock effects =
    Rock.Rock Elara.Query.Query ': effects
