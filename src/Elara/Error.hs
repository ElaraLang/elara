{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.Error where

import Error.Diagnose
import Prelude hiding (Reader, asks, readFile)

class ReportableError e where
    report :: e -> Report Text

class ReportDiagnostic e where
    reportDiagnostic :: e -> Diagnostic Text

instance {-# OVERLAPPABLE #-} ReportableError e => ReportDiagnostic e where
    reportDiagnostic = addReport def . report

collectErrors :: [Either (Diagnostic Text) a] -> Either (Diagnostic Text) [a]
collectErrors (partitionEithers -> partitioned) =
    case partitioned of
        ([], xs) -> Right xs
        (es, _) -> Left (foldr (<>) def es)

addPosition :: (Position, Marker msg) -> Report msg -> Report msg
addPosition marker (Err code m markers notes) = Err code m (marker : markers) notes
addPosition marker (Warn code m markers notes) = Warn code m (marker : markers) notes

-- | Concatenate two diagnostics, keeping the first one's file map. Use this instead of the Semigroup instance for Diagnostics.
concatDiagnostics :: Diagnostic msg -> Diagnostic msg -> Diagnostic msg
concatDiagnostics diag = (<> diag)
