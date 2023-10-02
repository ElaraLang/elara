module Elara.ReadFile where

import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Error.Diagnose hiding (addFile)
import Polysemy
import Polysemy.Error

type ReadFilePipelineEffects = '[Embed IO, Error ReadFileError, DiagnosticWriter (Doc AnsiStyle)]

data ReadFileError = DecodeError FilePath UnicodeException

instance ReportableError ReadFileError where
    report (DecodeError path _) =
        writeReport $
            Err
                (Just Codes.fileReadError)
                ("Couldn't read file " <> pretty path)
                []
                []

readFileString :: (Members ReadFilePipelineEffects r) => FilePath -> Sem r String
readFileString path = do
    contentsBS <- readFileBS path
    case decodeUtf8Strict contentsBS of
        Left ue -> throw (DecodeError path ue)
        Right contents -> do
            addFile path contents -- add the file to the diagnostic writer for nicer error messages
            pure contents

runReadFilePipeline :: (IsPipeline r) => Sem (EffectsAsPrefixOf ReadFilePipelineEffects r) a -> Sem r a
runReadFilePipeline =
    runErrorOrReport @ReadFileError . subsume_
