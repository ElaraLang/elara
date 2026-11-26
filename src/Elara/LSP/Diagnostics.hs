{-# LANGUAGE RecordWildCards #-}

module Elara.LSP.Diagnostics where

import Language.LSP.Protocol.Types qualified as LSP

import Elara.Data.Pretty (prettyToUnannotatedText)
import Elara.Error (ElaraMarker (..), ElaraMarkerType (..), ElaraNote (..), ElaraReport (..), ElaraSeverity (..))
import Elara.LSP.Position

-- | Convert an 'ElaraReport' to an LSP 'Diagnostic'
reportToDiagnostic :: (FilePath -> Maybe Text) -> ElaraReport -> LSP.Diagnostic
reportToDiagnostic readFile ElaraReport{..} = do
    let _range = case filter (\marker -> marker.markerType == PrimaryMarker) reportMarkers of
            (firstPrimary : _) -> regionToRangeIn readFile firstPrimary.markerRegion
            _ -> zeroLSPRange

    let _code = LSP.InR . show <$> reportCode
    let _severity = case reportSeverity of
            ErrorSeverity -> Just LSP.DiagnosticSeverity_Error
            WarningSeverity -> Just LSP.DiagnosticSeverity_Warning

    let _source = Just "elara"
    let _message = prettyToUnannotatedText reportMessage <> prettyNotesAndHints
          where
            prettyNotesAndHints = unlines (map prettyNote reportNotes)
            prettyNote (Note doc) = "**Note**: " <> prettyToUnannotatedText doc
            prettyNote (Hint doc) = "**Hint**: " <> prettyToUnannotatedText doc

    let _codeDescription = Nothing -- what is this?
    let _tags = Nothing
    let _data_ = Nothing
    let _relatedInformation = Nothing -- todo
    LSP.Diagnostic{..}
