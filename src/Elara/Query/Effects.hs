{- | Common module with query effects
To avoid a circular import we don't include the 'Rock' effect here even though it's commonly used
sorry :)
-}
module Elara.Query.Effects where

import Data.Kind (Constraint)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.FileSystem
import Elara.Data.Pretty
import Elara.Data.Unique.Effect
import Elara.Error
import Elara.Logging

type MinimumQueryEffects = ConsMinimumQueryEffects '[]

{- | The bare minimum effects that need to be present for any query,
even a "pure" one
-}
type ConsMinimumQueryEffects :: [Effect] -> [Effect]
type ConsMinimumQueryEffects es =
    Concurrent ': StructuredDebug ': es

type HasMinimumQueryEffects es =
    ( Concurrent :> es
    , StructuredDebug :> es
    )

type StandardQueryEffects = ConsQueryEffects '[]

-- | Standard effects that almost every query will use
type ConsQueryEffects :: [Effect] -> [Effect]
type ConsQueryEffects es =
    FileSystem
        ': Error SomeReportableError
        ': DiagnosticWriter (Doc AnsiStyle)
        ': UniqueGen
        ': StructuredDebug
        ': ConsMinimumQueryEffects es

type QueryEffects :: [Effect] -> Constraint
type QueryEffects es =
    ( FileSystem :> es
    , Concurrent :> es
    , Error SomeReportableError :> es
    , DiagnosticWriter (Doc AnsiStyle) :> es
    , UniqueGen :> es
    , StructuredDebug :> es
    )
