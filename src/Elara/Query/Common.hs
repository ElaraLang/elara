module Elara.Query.Common where

import Elara.Query qualified
import Rock qualified

{- | Appends 'Rock' to a list of effects.
  This type mainly exists to avoid a cyclic import between this module and 'Elara.Query.Effects'
-}
type WithRock effects =
    Rock.Rock Elara.Query.Query ': effects
