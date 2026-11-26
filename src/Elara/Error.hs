{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Errors, Warnings, and Diagnostics
module Elara.Error (
    ElaraDiagnostic (..),
    ElaraError (..),
    ElaraWarning (..),
    ElaraSeverity (..),
    ElaraMarker (..),
    ElaraMarkerType (..),
    ElaraNote (..),
    ElaraReport (..),
    dedupeReports,
    reportElaraWarning,
    runErrorAsElaraError,
    runWriterAsElaraWarning,
    throwElaraError,
    rethrowError,
    rethrowErrorWith,
    module Elara.Error.Codes,
)
where

import Effectful (Eff, (:>))

import Effectful.Error.Static qualified as Eff
import Effectful.Writer.Static.Local qualified as Eff
import GHC.Show qualified

import Elara.AST.Region (SourceRegion)
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

    diagnosticMessage :: e -> Doc Ann
    default diagnosticMessage :: Pretty e => e -> Doc Ann
    diagnosticMessage = pretty

    diagnosticMarkers :: e -> [ElaraMarker]
    diagnosticMarkers = const []

    diagnosticNotes :: e -> [ElaraNote]
    diagnosticNotes = const []

data ElaraSeverity = ErrorSeverity | WarningSeverity
    deriving (Eq, Ord, Show)

data ElaraMarker = ElaraMarker
    { markerRegion :: SourceRegion
    , markerType :: ElaraMarkerType
    , markerMessage :: Doc Ann
    }
    deriving (Generic, Show)

data ElaraMarkerType = PrimaryMarker | SecondaryMarker | InfoMarker
    deriving (Eq, Show)

data ElaraNote = Note (Doc Ann) | Hint (Doc Ann)
    deriving (Generic, Show)

data ElaraReport = ElaraReport
    { reportSeverity :: ElaraSeverity
    , reportCode :: Maybe ErrorCode
    , reportMessage :: Doc Ann
    , reportMarkers :: [ElaraMarker]
    , reportNotes :: [ElaraNote]
    }
    deriving (Generic, Show)

{- | Remove duplicate reports, needed since one compiler run can potentially report the same error
multiple times, e.g. once when it is thrown and once when a memoised failure is rethrown.

TODO: revisit and evaluate if this is really necessary
-}
dedupeReports :: [ElaraReport] -> [ElaraReport]
dedupeReports = ordNubOn key
  where
    key r =
        ( reportSeverity r
        , prettyToUnannotatedText <$> reportCode r
        , prettyToUnannotatedText (reportMessage r)
        , markerRegion <$> reportMarkers r
        )

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
    rethrowErrorWith ElaraError r

-- | Rethrow an 'Error', using its _original_ 'CallStack' rather than the current one
rethrowError :: (Eff.Error e :> es, Show e) => Either (CallStack, e) r -> Eff es r
rethrowError = rethrowErrorWith identity

-- | Rethrow an 'Error' wrapped with some function 'f', using its _original_ 'CallStack' rather than the current one
rethrowErrorWith :: (Eff.Error e2 :> es, Show e2) => (e -> e2) -> Either (CallStack, e) r -> Eff es r
rethrowErrorWith f (Left (_callStack, err)) = let ?callStack = _callStack in Eff.throwError (f err)
rethrowErrorWith _f (Right p) = pure p

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

throwElaraError :: (Exception e, ElaraDiagnostic e, Eff.Error ElaraError :> es, HasCallStack) => e -> Eff es a
throwElaraError = Eff.throwError . ElaraError
