{-# LANGUAGE RecordWildCards #-}

module Elara.Error.Diagnose where

import Elara.AST.Region (HasPath (path), sourceRegionToDiagnosePosition)
import Elara.Data.Pretty
import Elara.Error
import Error.Diagnose qualified as Diag

-- | Convert an 'ElaraDiagnostic' to a list of 'diagnose' 'Report's.
toDiagnoseReports :: ElaraDiagnostic e => e -> [Diag.Report (Doc AnsiStyle)]
toDiagnoseReports = fmap elaraReportToDiagnoseReport . diagnosticReports

-- | Convert a single 'ElaraReport' to a 'diagnose' 'Report'.
elaraReportToDiagnoseReport :: ElaraReport -> Diag.Report (Doc AnsiStyle)
elaraReportToDiagnoseReport ElaraReport{..} =
    let markers = fmap elaraMarkerToDiagnoseMarker reportMarkers
        notes = fmap elaraNoteToDiagnoseNote reportNotes
        constructor = case reportSeverity of
            ErrorSeverity -> Diag.Err
            WarningSeverity -> Diag.Warn
     in constructor (fmap (\(ErrorCode d) -> reAnnotate (const mempty) d) reportCode) reportMessage markers notes

-- | Convert an 'ElaraMarker' to a 'diagnose' marker.
elaraMarkerToDiagnoseMarker :: ElaraMarker -> (Diag.Position, Diag.Marker (Doc AnsiStyle))
elaraMarkerToDiagnoseMarker ElaraMarker{..} =
    let pos = sourceRegionToDiagnosePosition markerRegion
        marker = case markerType of
            PrimaryMarker -> Diag.This
            SecondaryMarker -> Diag.Where
            InfoMarker -> Diag.Maybe
     in (pos, marker markerMessage)

-- | Convert an 'ElaraNote' to a 'diagnose' note.
elaraNoteToDiagnoseNote :: ElaraNote -> Diag.Note (Doc AnsiStyle)
elaraNoteToDiagnoseNote (Note msg) = Diag.Note msg
elaraNoteToDiagnoseNote (Hint msg) = Diag.Hint msg

reportsToDiagnostic :: [ElaraReport] -> IO (Diag.Diagnostic (Doc AnsiStyle))
reportsToDiagnostic reports = do
    let emptyDiag = mempty
    let filesNeeded = catMaybes $ ordNub [markerRegion m ^. path | r <- reports, m <- reportMarkers r]
    fileContents <- for filesNeeded $ \fp -> do
        content <- readFileBS fp
        pure (fp, decodeUtf8 content)
    let diagWithFiles = foldl' (\d (fp, content) -> Diag.addFile d fp content) emptyDiag fileContents
    pure $ foldl' Diag.addReport diagWithFiles (fmap elaraReportToDiagnoseReport reports)
