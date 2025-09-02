{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Elara.Error (ReportableError (..), SomeReportableError (..), runErrorOrReportEff, defaultReport, addPosition, concatDiagnostics, module Elara.Error.Effect, runErrorOrReport, reportMaybe) where

import Effectful (Eff, (:>))
import Effectful.Error.Static qualified as Eff
import Elara.Data.Pretty
import Elara.Error.Codes
import Elara.Error.Effect
import Elara.Error.EffectNew qualified as Eff
import Error.Diagnose
import GHC.Show (Show (show))
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Maybe (MaybeE, justE, nothingE)
import Prelude hiding (asks, readFile)

class ReportableError e where
    errorCode :: e -> Maybe ErrorCode
    errorCode = const Nothing

    getReport :: e -> Maybe (Report (Doc AnsiStyle))
    getReport = const Nothing

    report :: Member (DiagnosticWriter (Doc AnsiStyle)) r => e -> Sem r ()
    default report :: Pretty e => Member (DiagnosticWriter (Doc AnsiStyle)) r => e -> Sem r ()
    report = defaultReport

defaultReport ::
    (ReportableError e, Pretty e) =>
    Member (DiagnosticWriter (Doc AnsiStyle)) r =>
    e -> Sem r ()
defaultReport e =
    {-# HLINT ignore "Use id" #-}
    -- i love impredicative types
    let code = (\x -> x) <$> errorCode e
        report = getReport e
     in writeReport (fromMaybe (Err code (pretty e) [] []) report)

data SomeReportableError = forall x. ReportableError x => SomeReportableError x

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
    x <- raise_ (runError e)
    case x of
        Left err -> report err *> nothingE
        Right a -> justE a

runErrorOrReportEff ::
    forall e r a.
    ( Eff.DiagnosticWriter (Doc AnsiStyle) :> r
    , Eff.Error SomeReportableError :> r
    , ReportableError e
    ) =>
    Eff (Eff.Error e ': r) a ->
    Eff r a
runErrorOrReportEff e = withFrozenCallStack $ do
    x <- Eff.runError e
    case x of
        Left (callStack, err) -> do
            let ?callStack = callStack -- silly
             in Eff.throwError_ (SomeReportableError err)
        Right a -> pure a

reportMaybe ::
    Member MaybeE r =>
    Member (DiagnosticWriter (Doc AnsiStyle)) r =>
    ReportableError e =>
    Sem r (Either e a) ->
    Sem r a
reportMaybe x = do
    x' <- x
    case x' of
        Left err -> report err *> nothingE
        Right a -> justE a
