{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Errors, Warnings, and Diagnostics
module Elara.Error (
    -- * New Error System
    ElaraDiagnostic (..),
    ElaraError (..),
    ElaraWarning (..),
    ElaraSeverity (..),
    ElaraMarker (..),
    ElaraMarkerType (..),
    ElaraNote (..),
    ElaraReport (..),
    reportElaraWarning,
    runErrorAsElaraError,
    runWriterAsElaraWarning,
    module Elara.Error.Codes,
)
where

import Effectful (Eff, (:>))
import Error.Diagnose hiding (Hint, Note)

import Effectful.Error.Static qualified as Eff
import Effectful.Writer.Static.Local qualified as Eff
import Error.Diagnose qualified as Diag
import GHC.Show qualified

import Elara.AST.Region (SourceRegion, sourceRegionToDiagnosePosition)
import Elara.Data.Pretty
import Elara.Error.Codes
import Prelude hiding (asks, readFile)

-- | A data-focused diagnostic that can be rendered to various formats (terminal, LSP, etc.)
class Typeable e => ElaraDiagnostic e where
    diagnosticReports :: e -> [ElaraReport]
    default diagnosticReports :: e -> [ElaraReport]
    diagnosticReports e = [ElaraReport (diagnosticSeverity e) (diagnosticCode e) (diagnosticMessage e) (diagnosticMarkers e) (diagnosticNotes e)]

    diagnosticSeverity :: e -> ElaraSeverity
    diagnosticSeverity = const ErrorSeverity

    diagnosticCode :: e -> Maybe ErrorCode
    diagnosticCode = const Nothing

    diagnosticMessage :: e -> Doc AnsiStyle
    default diagnosticMessage :: Pretty e => e -> Doc AnsiStyle
    diagnosticMessage = pretty

    diagnosticMarkers :: e -> [ElaraMarker]
    diagnosticMarkers = const []

    diagnosticNotes :: e -> [ElaraNote]
    diagnosticNotes = const []

data ElaraSeverity = ErrorSeverity | WarningSeverity
    deriving (Eq, Show)

data ElaraMarker = ElaraMarker
    { markerRegion :: SourceRegion
    , markerType :: ElaraMarkerType
    , markerMessage :: Doc AnsiStyle
    }
    deriving (Generic, Show)

data ElaraMarkerType = PrimaryMarker | SecondaryMarker | InfoMarker
    deriving (Eq, Show)

data ElaraNote = Note (Doc AnsiStyle) | Hint (Doc AnsiStyle)
    deriving (Generic, Show)

data ElaraReport = ElaraReport
    { reportSeverity :: ElaraSeverity
    , reportCode :: Maybe ErrorCode
    , reportMessage :: Doc AnsiStyle
    , reportMarkers :: [ElaraMarker]
    , reportNotes :: [ElaraNote]
    }
    deriving (Generic, Show)

-- | A type-erased error that implements 'ElaraDiagnostic' and 'Exception'.
data ElaraError = forall e. (Exception e, ElaraDiagnostic e) => ElaraError e

instance Show ElaraError where
    show (ElaraError e) = GHC.Show.show e

instance Exception ElaraError

instance ElaraDiagnostic ElaraError where
    diagnosticReports (ElaraError e) = diagnosticReports e
    diagnosticMessage (ElaraError e) = diagnosticMessage e
    diagnosticSeverity (ElaraError e) = diagnosticSeverity e
    diagnosticCode (ElaraError e) = diagnosticCode e
    diagnosticMarkers (ElaraError e) = diagnosticMarkers e
    diagnosticNotes (ElaraError e) = diagnosticNotes e

instance Pretty ElaraError where
    pretty (ElaraError e) = diagnosticMessage e

-- | A type-erased warning that implements 'ElaraDiagnostic'.
data ElaraWarning = forall w. (Typeable w, ElaraDiagnostic w) => ElaraWarning w

instance ElaraDiagnostic ElaraWarning where
    diagnosticReports (ElaraWarning w) = diagnosticReports w
    diagnosticMessage (ElaraWarning w) = diagnosticMessage w
    diagnosticSeverity (ElaraWarning w) = diagnosticSeverity w
    diagnosticCode (ElaraWarning w) = diagnosticCode w
    diagnosticMarkers (ElaraWarning w) = diagnosticMarkers w
    diagnosticNotes (ElaraWarning w) = diagnosticNotes w

instance Pretty ElaraWarning where
    pretty (ElaraWarning w) = diagnosticMessage w

reportElaraWarning :: (Typeable w, ElaraDiagnostic w, Eff.Writer [ElaraWarning] :> es) => w -> Eff es ()
reportElaraWarning w = Eff.tell [ElaraWarning w]

runErrorAsElaraError ::
    forall e r a.
    (Exception e, ElaraDiagnostic e, Eff.Error ElaraError :> r) =>
    Eff (Eff.Error e ': r) a ->
    Eff r a
runErrorAsElaraError e = withFrozenCallStack $ do
    r <- Eff.runError e
    case r of
        Left (_callStack, err) -> let ?callStack = _callStack in Eff.throwError (ElaraError err)
        Right a -> pure a

runWriterAsElaraWarning ::
    forall w r a f.
    ( Typeable w
    , ElaraDiagnostic w
    , Eff.Writer [ElaraWarning] :> r
    , (Monoid (f w))
    , Foldable f
    ) =>
    Eff (Eff.Writer (f w) ': r) a ->
    Eff r a
runWriterAsElaraWarning m = do
    (result, specificWarnings) <- Eff.runWriter m

    Eff.tell (ElaraWarning <$> toList specificWarnings)

    pure result
