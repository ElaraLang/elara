module Elara.ReadFile (FileContents (FileContents, fileContents), ReadFileError, ModulePathError (..), runGetFileContentsQuery, getInputFiles, findElaraFiles) where

import Data.HashSet qualified as HashSet
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, listDirectory)
import Effectful.FileSystem qualified as Eff
import Effectful.FileSystem.IO.ByteString qualified as Eff
import Elara.AST.Name (ModuleName)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Settings
import Error.Diagnose hiding (addFile)
import Print (showPretty)
import System.FilePath (takeExtension, (</>))

data ReadFileError
    = DecodeError !FilePath !UnicodeException
    | FileNotFound !FilePath
    deriving (Show)

data ModulePathError
    = ModuleNotFound !ModuleName ![FilePath]
    | MultipleModulePaths !ModuleName ![FilePath]
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

instance ReportableError ModulePathError where
    report (ModuleNotFound mn paths) =
        writeReport $
            Err
                (Just Codes.fileNotFound)
                ("Module not found: " <> pretty mn)
                []
                [Note $ "Looked in: " <> pretty (showPretty paths)]
    report (MultipleModulePaths mn paths) =
        writeReport $
            Err
                (Just Codes.ambiguousModulePath)
                ("Multiple paths found for module: " <> pretty mn)
                []
                [Note $ "Found in: " <> pretty (showPretty paths)]

getInputFiles :: CompilerSettings -> Eff '[FileSystem] (HashSet FilePath)
getInputFiles settings = do
    createDirectoryIfMissing True "stdlib"
    stdlib <- fmap ("stdlib/" <>) <$> listDirectory "stdlib"
    let source = fromMaybe "source.elr" settings.mainFile

    pure $ HashSet.fromList (stdlib <> [source])

findElaraFiles :: FileSystem :> es => [FilePath] -> Eff es [FilePath]
findElaraFiles roots = concat <$> traverse findElaraFiles' roots

findElaraFiles' :: FileSystem :> es => FilePath -> Eff es [FilePath]
findElaraFiles' root = do
    exists <- Eff.doesDirectoryExist root
    if not exists
        then pure []
        else do
            items <- Eff.listDirectory root
            paths <- for items $ \item -> do
                let path = root </> item
                isDir <- Eff.doesDirectoryExist path
                if isDir
                    then findElaraFiles' path
                    else
                        if takeExtension path == ".elr"
                            then pure [path]
                            else pure []
            pure $ concat paths

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
