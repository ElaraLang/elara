{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.Error where

import Control.Exception (IOException, catch)
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

instance ReportableError e => ReportDiagnostic e where
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
    -- TODO this is probably not very efficient
    let
        segment = T.take (end - start) (T.drop start fileContents)
        lineNumberOfStart = T.count "\n" (T.take start fileContents) + 1
        startColumn = T.length $ last $ fromList $ T.splitOn "\n" $ T.take start fileContents
        lineNumberOfEnd = lineNumberOfStart + T.count "\n" segment
        endColumn = T.length $ last $ fromList $ T.splitOn "\n" $ T.take end fileContents
     in
        (SourcePos fp (mkPos lineNumberOfStart) (mkPos (startColumn + 1)), SourcePos fp (mkPos lineNumberOfEnd) (mkPos (endColumn + 1)))

sourceRegionToPosition :: Member FileContents r => SourceRegion -> Sem r Position
sourceRegionToPosition sr@(SourceRegion fp _ _) = do
    (SourcePos _ startLine startColumn, SourcePos _ endLine endColumn) <- sourceRegionToSourcePos sr
    pure (Position (unPos startLine, unPos startColumn) (unPos endLine, unPos endColumn) (fromMaybe "<unknown>" fp))