module Elara.ReadFile where

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Search qualified as S
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Error.Diagnose hiding (addFile)
import Polysemy
import Polysemy.Error
import System.IO

-- https://stackoverflow.com/a/6860159
nativeCallsForConversion :: Bool
nativeCallsForConversion = System.IO.nativeNewline == System.IO.CRLF

readFileUniversalNewlineConversion :: FilePath -> IO L.ByteString
readFileUniversalNewlineConversion =
    let
        str_LF = B.pack [10]
        str_CRLF = B.pack [13, 10]
     in
        fmap (S.replace str_CRLF str_LF) . readFileLBS
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

readFileString :: Members ReadFilePipelineEffects r => FilePath -> Sem r String
readFileString path = do
    contentsBS <- embed $ readFileUniversalNewlineConversion path
    case decodeUtf8Strict contentsBS of
        Left ue -> throw (DecodeError path ue)
        Right contents -> do
            addFile path contents -- add the file to the diagnostic writer for nicer error messages
            pure contents

runReadFilePipeline :: IsPipeline r => Sem (EffectsAsPrefixOf ReadFilePipelineEffects r) a -> Sem r a
runReadFilePipeline =
    runErrorOrReport @ReadFileError . subsume_
