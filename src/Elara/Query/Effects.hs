{- | Common module with query effects
To avoid a circular import we don't include the 'Rock' effect here even though it's commonly used
sorry :)
-}
module Elara.Query.Effects where

import Data.Kind (Constraint)
import Effectful
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Writer.Static.Local

import Elara.Data.Unique.Effect
import Elara.Error
import Elara.Logging

-- | The effects every query runs with
type QueryEffects :: [Effect] -> Constraint
type QueryEffects es =
    ( FileSystem :> es
    , Error ElaraError :> es
    , Writer [ElaraWarning] :> es
    , UniqueGen :> es
    , StructuredDebug :> es
    )

-- | 'QueryEffects' as a concrete effect list, consed onto @es@
type ConsQueryEffects :: [Effect] -> [Effect]
type ConsQueryEffects es =
    FileSystem
        ': Error ElaraError
        ': Writer [ElaraWarning]
        ': UniqueGen
        ': StructuredDebug
        ': es
