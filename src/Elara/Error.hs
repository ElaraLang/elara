{-# LANGUAGE UndecidableInstances #-}

module Elara.Error (ReportableError (..), addPosition, concatDiagnostics, module Elara.Error.Effect, runErrorOrReport, reportMaybe) where

import Elara.Data.Pretty
import Elara.Error.Effect
import Error.Diagnose
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Maybe (MaybeE, justE, nothingE)
import Prelude hiding (asks, readFile)

class ReportableError e where
    report :: (Member (DiagnosticWriter (Doc AnsiStyle)) r) => e -> Sem r ()

addPosition :: (Position, Marker msg) -> Report msg -> Report msg
addPosition marker (Err code m markers notes) = Err code m (marker : markers) notes
addPosition marker (Warn code m markers notes) = Warn code m (marker : markers) notes

-- | Concatenate two diagnostics, keeping the first one's file map. Use this instead of the Semigroup instance for Diagnostics.
concatDiagnostics :: Diagnostic msg -> Diagnostic msg -> Diagnostic msg
concatDiagnostics diag = (<> diag)

runErrorOrReport ::
    forall e r a.
    (Members '[DiagnosticWriter (Doc AnsiStyle), MaybeE] r, ReportableError e) =>
    Sem (Error e ': r) a ->
    Sem r a
runErrorOrReport e = do
    x <- subsume_ (runError e)
    case x of
        Left err -> report err *> nothingE
        Right a -> justE a

reportMaybe ::
    Member MaybeE r =>
    Member (DiagnosticWriter (Doc AnsiStyle)) r =>
    (ReportableError e) =>
    Sem r (Either e a) ->
    Sem r a
reportMaybe x = do
    x' <- x
    case x' of
        Left err -> report err *> nothingE
        Right a -> justE a
