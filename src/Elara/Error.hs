{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Errors, Warnings, and Diagnostics
module Elara.Error (
    ReportableError (..),
    SomeReportableError (..),
    runErrorOrReport,
    defaultReport,
    addPosition,
    concatDiagnostics,
    module Elara.Error.Effect,
) where

import Effectful (Eff, (:>))
import Effectful.Error.Static qualified as Eff
import Elara.Data.Pretty
import Elara.Error.Codes
import Elara.Error.Effect
import Error.Diagnose
import Prelude hiding (asks, readFile)

class ReportableError e where
    errorCode :: e -> Maybe ErrorCode
    errorCode = const Nothing

    getReport :: e -> Maybe (Report (Doc AnsiStyle))
    getReport = const Nothing

    report :: DiagnosticWriter (Doc AnsiStyle) :> r => e -> Eff r ()
    default report :: (Pretty e, DiagnosticWriter (Doc AnsiStyle) :> r) => e -> Eff r ()
    report = defaultReport

defaultReport ::
    (ReportableError e, Pretty e, DiagnosticWriter (Doc AnsiStyle) :> r) =>
    e -> Eff r ()
defaultReport e =
    {-# HLINT ignore "Use id" #-}
    -- i love impredicative types
    let code = (\x -> x) <$> errorCode e
        report = getReport e
     in writeReport (fromMaybe (Err code (pretty e) [] []) report)

data SomeReportableError = forall x. ReportableError x => SomeReportableError x
instance ReportableError SomeReportableError where
    errorCode (SomeReportableError e) = errorCode e
    getReport (SomeReportableError e) = getReport e
    report (SomeReportableError e) = report e

addPosition :: (Position, Marker msg) -> Report msg -> Report msg
addPosition marker (Err code m markers notes) = Err code m (marker : markers) notes
addPosition marker (Warn code m markers notes) = Warn code m (marker : markers) notes

-- | Concatenate two diagnostics, keeping the first one's file map. Use this instead of the Semigroup instance for Diagnostics.
concatDiagnostics :: Diagnostic msg -> Diagnostic msg -> Diagnostic msg
concatDiagnostics diag = (<> diag)

runErrorOrReport ::
    forall e r a.
    ( DiagnosticWriter (Doc AnsiStyle) :> r
    , Eff.Error SomeReportableError :> r
    , ReportableError e
    ) =>
    Eff (Eff.Error e ': r) a ->
    Eff r a
runErrorOrReport e = withFrozenCallStack $ do
    x <- Eff.runError e
    case x of
        Left (callStack, err) -> do
            let ?callStack = callStack -- silly
             in Eff.throwError_ (SomeReportableError err)
        Right a -> pure a

class ReportableWarning w where
    warningCode :: w -> Maybe ErrorCode
    warningCode = const Nothing

    getWarningReport :: w -> Maybe (Report (Doc AnsiStyle))
    getWarningReport = const Nothing

    reportWarning :: DiagnosticWriter (Doc AnsiStyle) :> r => w -> Eff r ()
    default reportWarning :: (Pretty w, DiagnosticWriter (Doc AnsiStyle) :> r) => w -> Eff r ()
    reportWarning = defaultReportWarning

defaultReportWarning ::
    (ReportableWarning w, Pretty w, DiagnosticWriter (Doc AnsiStyle) :> r) =>
    w -> Eff r ()
defaultReportWarning w =
    let code = (\x -> x) <$> warningCode w
        report = getWarningReport w
     in writeReport (fromMaybe (Warn code (pretty w) [] []) report)

data SomeReportableWarning = forall x. ReportableWarning x => SomeReportableWarning x

instance ReportableWarning SomeReportableWarning where
    warningCode (SomeReportableWarning w) = warningCode w
    getWarningReport (SomeReportableWarning w) = getWarningReport w
    reportWarning (SomeReportableWarning w) = reportWarning w
