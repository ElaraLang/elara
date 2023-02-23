{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.Error where

import Data.Map (lookup)
import Data.Text qualified as T
import Elara.AST.Region (SourceRegion (SourceRegion))
import Error.Diagnose
import Polysemy
import Polysemy.Reader
import Relude.Unsafe (fromJust)
import Text.Megaparsec (SourcePos (..), unPos)
import Text.Megaparsec.Pos (mkPos)
import Prelude hiding (Reader, asks, readFile)

class ReportableError e where
    report :: Member FileContents r => e -> Sem r (Report Text)

class ReportDiagnostic e where
    reportDiagnostic :: Member FileContents r => e -> Sem r (Diagnostic Text)

instance {-# OVERLAPPABLE #-} ReportableError e => ReportDiagnostic e where
    reportDiagnostic = fmap (addReport def) . report

collectErrors :: [Either (Diagnostic Text) a] -> Either (Diagnostic Text) [a]
collectErrors (partitionEithers -> partitioned) =
    case partitioned of
        ([], xs) -> Right xs
        (es, _) -> Left (foldr (<>) def es)

data FileContents m a where
    GetContents :: FilePath -> FileContents m (Maybe Text)

makeSem ''FileContents

runFileContentsPure :: Member (Reader (Map FilePath Text)) r => Sem (FileContents ': r) a -> Sem r a
runFileContentsPure = interpret $ \case
    GetContents fp -> asks (lookup fp)

runFileContentsIO :: Member (Embed IO) r => Sem (FileContents ': r) a -> Sem r a
runFileContentsIO = interpret $ \case
    GetContents fp -> embed $ do
        c <- decodeUtf8Strict <$> readFileBS fp
        case c of
            Left _ -> pure Nothing
            Right t -> pure (Just t)

sourceRegionToSourcePos :: Member FileContents r => SourceRegion -> Sem r (SourcePos, SourcePos)
sourceRegionToSourcePos (SourceRegion Nothing _ _) = error "sourceRegionToSourcePos: SourceRegion has no file path"
sourceRegionToSourcePos sr@(SourceRegion (Just fp) _ _) = do
    fileContents <- fromJust <$> getContents fp
    pure (sourceRegionToSourcePos' fileContents sr)

sourceRegionToSourcePos' :: Text -> SourceRegion -> (SourcePos, SourcePos)
sourceRegionToSourcePos' _ (SourceRegion Nothing _ _) = error "sourceRegionToSourcePos: SourceRegion has no file path"
sourceRegionToSourcePos' fileContents (SourceRegion (Just fp) start end) =
    let
        startSegment = T.take start fileContents -- take the whole file up to the start of the region
        segment = T.take (end - start) (T.drop start fileContents) -- take the region
        startLine = T.count "\n" startSegment + 1 -- count the number of newlines in the start segment, and add 1 to get the line number
        endLine = startLine + T.count "\n" segment -- count the number of newlines in the region, and add the start line to get the end line
        startColumn =
            startSegment
                & T.splitOn "\n"
                & fromList
                & last
                & T.length
        endColumn =
            fileContents
                & T.take end -- take the whole file up to the end of the region
                & T.splitOn "\n" -- split into lines
                & fromList -- convert to nonempty list, it should never be empty
                & last -- get the last line, i.e. the line containing the end of the region
                & T.dropWhileEnd (== ' ') -- drop the trailing whitespace. kinda a hack but it's easier than changing how parsing works
                & T.length -- get the length of the line
     in
        (SourcePos fp (mkPos startLine) (mkPos (startColumn + 1)), SourcePos fp (mkPos endLine) (mkPos (endColumn + 1)))

sourceRegionToPosition :: Member FileContents r => SourceRegion -> Sem r Position
sourceRegionToPosition sr@(SourceRegion fp _ _) = do
    (SourcePos _ startLine startColumn, SourcePos _ endLine endColumn) <- sourceRegionToSourcePos sr
    pure (Position (unPos startLine, unPos startColumn) (unPos endLine, unPos endColumn) (fromMaybe "<unknown>" fp))

addPosition :: (Position, Marker msg) -> Report msg -> Report msg
addPosition marker (Err code m markers notes) = Err code m (marker : markers) notes
addPosition marker (Warn code m markers notes) = Warn code m (marker : markers) notes