{-# LANGUAGE UndecidableInstances #-}

module Elara.Error (ReportableError (..), addPosition, concatDiagnostics, module Elara.Error.Effect) where

import Elara.Error.Effect
import Error.Diagnose
import Polysemy
import Prelude hiding (asks, readFile)

class ReportableError e where
    report :: (Member (DiagnosticWriter Text) r) => e -> Sem r ()

addPosition :: (Position, Marker msg) -> Report msg -> Report msg
addPosition marker (Err code m markers notes) = Err code m (marker : markers) notes
addPosition marker (Warn code m markers notes) = Warn code m (marker : markers) notes

-- | Concatenate two diagnostics, keeping the first one's file map. Use this instead of the Semigroup instance for Diagnostics.
concatDiagnostics :: Diagnostic msg -> Diagnostic msg -> Diagnostic msg
concatDiagnostics diag = (<> diag)
