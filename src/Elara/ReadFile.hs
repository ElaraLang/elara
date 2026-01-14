module Elara.ReadFile (FileContents (FileContents), runGetFileContentsQuery, getInputFiles) where

import Data.HashSet qualified as HashSet
import Effectful (Eff, (:>))
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, listDirectory)
import Effectful.FileSystem.IO.ByteString qualified as Eff
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Error.Diagnose hiding (addFile)

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
    createDirectoryIfMissing True "stdlib"
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
    { filePath :: !FilePath
    , fileContents :: !Text
    }
    deriving (Eq, Show, Ord, Generic)
instance Hashable FileContents
