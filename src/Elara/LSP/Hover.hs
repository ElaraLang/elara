-- | Answering hover requests
module Elara.LSP.Hover (hoverAt) where

import Data.Text qualified as Text
import Language.LSP.Protocol.Types qualified as LSP

import Elara.LSP.Check (FileSnapshot (..), ownReports)
import Elara.LSP.Diagnostics (primaryRegion, reportToMarkdown)
import Elara.LSP.Position (rangeContains, regionToRangeIn)

{- | What to show when hovering over a position in a checked file.
Currently this is the full report of every error and warning underlined at that position.
-}
hoverAt :: FileSnapshot -> LSP.Position -> Maybe LSP.Hover
hoverAt snapshot position = case mapMaybe hoverFor (ownReports snapshot) of
    [] -> Nothing
    found@((range, _) : _) ->
        Just
            LSP.Hover
                { LSP._contents =
                    LSP.InL
                        ( LSP.MarkupContent
                            LSP.MarkupKind_Markdown
                            (Text.intercalate "\n\n---\n\n" (snd <$> found))
                        )
                , LSP._range = Just range
                }
  where
    readFile' file =
        if file == snapshotPath snapshot
            then Just (snapshotText snapshot)
            else Nothing

    -- the range and the details of a report, if the report is underlined at the position
    hoverFor report = do
        region <- primaryRegion report
        let range = regionToRangeIn readFile' region
        guard (rangeContains range position)
        pure (range, reportToMarkdown readFile' report)
