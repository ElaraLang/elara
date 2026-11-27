{-# LANGUAGE RecordWildCards #-}

module Elara.LSP.Diagnostics (
    diagnosticSource,
    reportToDiagnostic,
    reportToMarkdown,
    reportPrimaryFile,
    reportFiles,
    reportLocation,
    regionLocation,
    primaryRegion,
) where

import Data.List (partition)

import Data.Text qualified as Text
import Language.LSP.Protocol.Types qualified as LSP

import Elara.AST.Region (RealSourceRegion (..), SourceRegion (..), line, path)
import Elara.Data.Pretty (prettyToUnannotatedText)
import Elara.Error (ElaraMarker (..), ElaraMarkerType (..), ElaraNote (..), ElaraReport (..), ElaraSeverity (..))
import Elara.LSP.Markdown (docToMarkdown, escapeMarkdown)
import Elara.LSP.Position

-- | The source that every diagnostic we report is attributed to.
diagnosticSource :: Text
diagnosticSource = "elara"

-- | A report's markers, sorted with the primary ones first
orderedMarkers :: ElaraReport -> [ElaraMarker]
orderedMarkers ElaraReport{reportMarkers} = primaries <> others
  where
    (primaries, others) = partition ((== PrimaryMarker) . markerType) reportMarkers

-- | The region a report is "about", based on its first primary marker if present, otherwise its first marker of any kind
primaryRegion :: ElaraReport -> Maybe SourceRegion
primaryRegion report = markerRegion <$> viaNonEmpty head (orderedMarkers report)

-- | The file that a report is about, if it is about a real location in one
reportPrimaryFile :: ElaraReport -> Maybe FilePath
reportPrimaryFile report = primaryRegion report >>= view path

-- | Every file that a report points into, without duplicates
reportFiles :: ElaraReport -> [FilePath]
reportFiles ElaraReport{reportMarkers} = ordNub (mapMaybe (view path . markerRegion) reportMarkers)

-- | A region's location if present, as an 'LSP.Location'.
regionLocation :: (FilePath -> Maybe Text) -> SourceRegion -> Maybe LSP.Location
regionLocation readFile region = do
    file <- view path region
    pure (LSP.Location (LSP.filePathToUri file) (regionToRangeIn readFile region))

-- | Where a report is about, as an 'LSP.Location'
reportLocation :: (FilePath -> Maybe Text) -> ElaraReport -> Maybe LSP.Location
reportLocation readFile report = primaryRegion report >>= regionLocation readFile

-- | Convert an 'ElaraReport' to an 'LSP.Diagnostic'
reportToDiagnostic ::
    -- | Look up the current contents of a file if known
    (FilePath -> Maybe Text) ->
    ElaraReport ->
    LSP.Diagnostic
reportToDiagnostic readFile report@ElaraReport{..} =
    LSP.Diagnostic
        { _range = maybe zeroLSPRange (regionToRangeIn readFile) (primaryRegion report)
        , _severity = Just $ case reportSeverity of
            ErrorSeverity -> LSP.DiagnosticSeverity_Error
            WarningSeverity -> LSP.DiagnosticSeverity_Warning
        , _code = LSP.InR . prettyToUnannotatedText <$> reportCode
        , _codeDescription = Nothing
        , _source = Just diagnosticSource
        , _message = mainMessage
        , _tags = Nothing
        , _relatedInformation = if null related then Nothing else Just related
        , _data_ = Nothing
        }
  where
    mainMessage = prettyToUnannotatedText reportMessage

    -- Places in other files that are relevant to the error, e.g. where a conflicting definition is.
    related = mapMaybe markerToRelated (filter inOtherFile (drop 1 (orderedMarkers report)))

    inOtherFile marker = view path (markerRegion marker) /= reportPrimaryFile report

    markerToRelated marker = do
        location <- regionLocation readFile (markerRegion marker)
        let text = prettyToUnannotatedText (markerMessage marker)
        pure (LSP.DiagnosticRelatedInformation location (if Text.null text then "related location" else text))

-- | Render a report as Markdown text for hover messages.
reportToMarkdown ::
    -- | Look up the current contents of a file if known
    (FilePath -> Maybe Text) ->
    ElaraReport ->
    Text
reportToMarkdown readFile report@ElaraReport{..} =
    Text.intercalate "\n\n" (catMaybes [Just header, snippet] <> markerBullets <> map noteMarkdown reportNotes)
  where
    severity = case reportSeverity of
        ErrorSeverity -> "error"
        WarningSeverity -> "warning"
    header =
        "**"
            <> severity
            <> maybe "" (\code -> "[" <> escapeMarkdown (prettyToUnannotatedText code) <> "]") reportCode
            <> "**: "
            <> docToMarkdown reportMessage

    -- the lines of code the report points at
    snippet = do
        RealSourceRegion (UnsafeMkSourceRegion file start end) <- primaryRegion report
        text <- file >>= readFile
        let codeLines = take (end ^. line - start ^. line + 1) . drop (start ^. line - 1) $ Text.splitOn "\n" text
        guard (not (all Text.null codeLines))
        pure ("```elara\n" <> Text.intercalate "\n" (Text.dropWhileEnd (== '\r') <$> codeLines) <> "\n```")

    markerBullets = case filter (not . Text.null . prettyToUnannotatedText . markerMessage) (orderedMarkers report) of
        [] -> []
        markers -> [Text.intercalate "\n" (("- " <>) . docToMarkdown . markerMessage <$> markers)]

    noteMarkdown (Note doc) = "**Note**: " <> docToMarkdown doc
    noteMarkdown (Hint doc) = "**Hint**: " <> docToMarkdown doc
