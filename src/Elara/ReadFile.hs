module Elara.ReadFile where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Search qualified as S
import Data.HashSet qualified as HashSet
import Effectful (Eff, (:>))
import Effectful.FileSystem (FileSystem, listDirectory)
import Effectful.FileSystem.IO.ByteString qualified as Eff
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Error.Diagnose hiding (addFile)
import Polysemy
import Polysemy.Error
import System.IO

-- https://stackoverflow.com/a/6860159
nativeCallsForConversion :: Bool
nativeCallsForConversion = System.IO.nativeNewline == System.IO.CRLF

readFileUniversalNewlineConversion :: FilePath -> IO L.ByteString
readFileUniversalNewlineConversion =
    let str_LF = B.pack [10]
        str_CRLF = B.pack [13, 10]
     in fmap (S.replace str_CRLF str_LF) . readFileLBS

readFileNativeNewlineConversion :: FilePath -> IO L.ByteString
readFileNativeNewlineConversion =
    if nativeCallsForConversion
        then readFileUniversalNewlineConversion
        else readFileLBS

type ReadFilePipelineEffects = '[Embed IO, Error ReadFileError, DiagnosticWriter (Doc AnsiStyle)]

data ReadFileError = DecodeError !FilePath !UnicodeException

instance ReportableError ReadFileError where
    report (DecodeError path _) =
        writeReport $
            Err
                (Just Codes.fileReadError)
                ("Couldn't read file " <> pretty path)
                []
                []

getInputFiles :: Eff '[FileSystem] (HashSet FilePath)
getInputFiles = do
    stdlib <- fmap ("stdlib/" <>) <$> listDirectory "stdlib"
    let source = "source.elr"

    pure $ HashSet.fromList (stdlib <> [source])

runGetFileContentsQuery :: (DiagnosticWriter (Doc AnsiStyle) :> es, FileSystem :> es) => FilePath -> Eff es FileContents
runGetFileContentsQuery fp = do
    contents <- Eff.readFile fp
    let contentsText = decodeUtf8 contents
    addFile fp (toString contentsText)
    pure $ FileContents fp contentsText

data FileContents = FileContents
    { filePath :: FilePath
    , fileContents :: Text
    }
    deriving (Eq, Show, Ord, Generic)
instance Hashable FileContents
