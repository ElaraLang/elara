module Elara.ReadFile (FileContents (FileContents), ReadFileError, runGetFileContentsQuery, getInputFiles) where

import Data.HashSet qualified as HashSet
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, listDirectory)
import Effectful.FileSystem qualified as Eff
import Effectful.FileSystem.IO.ByteString qualified as Eff
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Settings
import Error.Diagnose hiding (addFile)

data ReadFileError
    = DecodeError !FilePath !UnicodeException
    | FileNotFound !FilePath
    deriving (Show)

instance ReportableError ReadFileError where
    report (DecodeError path _) =
        writeReport $
            Err
                (Just Codes.fileReadError)
                ("Couldn't read file " <> pretty path)
                []
                []
    report (FileNotFound path) =
        writeReport $
            Err
                (Just Codes.fileNotFound)
                ("File not found: " <> Style.bold (Style.moduleName (pretty path)))
                []
                []

getInputFiles :: CompilerSettings -> Eff '[FileSystem] (HashSet FilePath)
getInputFiles settings = do
    createDirectoryIfMissing True "stdlib"
    stdlib <- fmap ("stdlib/" <>) <$> listDirectory "stdlib"
    let source = fromMaybe "source.elr" settings.mainFile

    pure $ HashSet.fromList (stdlib <> [source])

runGetFileContentsQuery ::
    ( DiagnosticWriter (Doc AnsiStyle) :> es
    , FileSystem :> es
    , Error ReadFileError :> es
    , HasCallStack
    ) =>
    FilePath -> Eff es FileContents
runGetFileContentsQuery fp = do
    unlessM (Eff.doesFileExist fp) $
        throwError $
            FileNotFound fp
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
