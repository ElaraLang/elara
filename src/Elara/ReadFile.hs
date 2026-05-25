module Elara.ReadFile (FileContents (FileContents, fileContents), ReadFileError, ModulePathError (..), runGetFileContentsQuery, getInputFiles, findElaraFiles) where

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing, listDirectory)
import System.FilePath (takeExtension, (</>))

import Data.HashSet qualified as HashSet
import Effectful.FileSystem qualified as Eff
import Effectful.FileSystem.IO.ByteString qualified as Eff

import Elara.AST.Name (ModuleName)
import Elara.Data.Pretty
import Elara.Error
import Elara.Settings
import Print (showPretty)

import Elara.Data.Pretty.Styles qualified as Style
import Elara.Error.Codes qualified as Codes

data ReadFileError
    = DecodeError !FilePath !UnicodeException
    | FileNotFound !FilePath
    deriving (Show)

data ModulePathError
    = ModuleNotFound !ModuleName ![FilePath]
    | MultipleModulePaths !ModuleName ![FilePath]
    deriving (Show)

instance Exception ReadFileError

instance ElaraDiagnostic ReadFileError where
    diagnosticMessage (DecodeError path _) = "Couldn't read file " <> pretty path
    diagnosticMessage (FileNotFound path) = "File not found: " <> Style.bold (Style.moduleName (pretty path))
    diagnosticCode (DecodeError _ _) = Just Codes.fileReadError
    diagnosticCode (FileNotFound _) = Just Codes.fileNotFound
    diagnosticMarkers _ = []
    diagnosticNotes _ = []

instance Exception ModulePathError

instance ElaraDiagnostic ModulePathError where
    diagnosticMessage (ModuleNotFound mn _) = "Module not found: " <> pretty mn
    diagnosticMessage (MultipleModulePaths mn _) = "Multiple paths found for module: " <> pretty mn
    diagnosticCode (ModuleNotFound _ _) = Just Codes.fileNotFound
    diagnosticCode (MultipleModulePaths _ _) = Just Codes.ambiguousModulePath
    diagnosticMarkers _ = []
    diagnosticNotes (ModuleNotFound _ paths) = [Elara.Error.Note $ "Looked in: " <> pretty (showPretty paths)]
    diagnosticNotes (MultipleModulePaths _ paths) = [Elara.Error.Note $ "Found in: " <> pretty (showPretty paths)]

instance Pretty ReadFileError where
    pretty = diagnosticMessage

instance Pretty ModulePathError where
    pretty = diagnosticMessage

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
    ( FileSystem :> es
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
    pure $ FileContents fp contentsText

data FileContents = FileContents
    { filePath :: !FilePath
    , fileContents :: !Text
    }
    deriving (Eq, Generic, Ord, Show)

instance Hashable FileContents
