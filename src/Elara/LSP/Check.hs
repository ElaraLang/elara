-- | Handles running the compiler on a single document and packaging up everything the language server wants to know about it
module Elara.LSP.Check (
    FileSnapshot (..),
    checkDocument,
    ownReports,
) where

import Colog.Core (LogAction (..))
import Control.Exception.Safe (tryAny)
import Effectful (Eff, runEff, (:>))
import Effectful.Error.Static (Error, runErrorNoCallStack, tryError)
import Effectful.Writer.Static.Local (runWriter)
import Relude.Extra (firstF)

import Effectful.Colog (runLogAction)
import Elara.AST.Module (Module (..), Module' (..))
import Elara.AST.Phases.Frontend (Frontend)
import Elara.AST.Phases.Typed (Typed)
import Elara.AST.Region (SourceRegion, unlocated)
import Elara.Data.Pretty (Ann, Doc)
import Elara.Error
import Elara.LSP.Diagnostics (reportPrimaryFile)
import Elara.ReadFile (FileContents (fileContents))
import Elara.Settings (CompilerSettings (..))

import Elara qualified
import Elara.Query qualified as Query
import Rock qualified

-- | Everything we learned from compiling one file
data FileSnapshot = FileSnapshot
    { snapshotPath :: FilePath
    -- ^ The absolute path of the file, which should also the path used in every 'SourceRegion' inside the file
    , snapshotText :: Text
    -- ^ The textual contents of the file
    , snapshotReports :: [ElaraReport]
    -- ^ Every error and warning, without duplicates. Some of these may be about other files.
    , snapshotParsed :: Maybe (Module SourceRegion Frontend)
    -- ^ The parsed AST, if we got that far
    , snapshotTyped :: Maybe (Module SourceRegion Typed)
    -- ^ The type-checked AST, if we got that far
    , snapshotBlocker :: Maybe ElaraError
    -- ^ The error that stopped us from fully processing the file, if there was one
    }

-- | The reports that belong in this file's own diagnostics, i.e. reports that match this file, and reports that aren't tied to a specific file
ownReports :: FileSnapshot -> [ElaraReport]
ownReports snapshot =
    filter
        (maybe True (== snapshotPath snapshot) . reportPrimaryFile)
        (snapshotReports snapshot)

-- | Compile one file and everything it imports and collect the results.
checkDocument :: CompilerSettings -> FilePath -> IO FileSnapshot
checkDocument base path = toSnapshot path <$> compileDocument settings path
  where
    settings = base{mainFile = Just path, dumpTargets = mempty}

-- | What compiling a document produced
data CompileResult = CompileResult
    { compiledText :: Text
    , compiledWarnings :: [ElaraWarning]
    , compiledOutcome :: CompileOutcome
    }

-- | How far compiling a document got
data CompileOutcome
    = -- | The file couldn't be parsed
      ParseFailed ElaraError
    | -- | The file was parsed, but couldn't be type checked
      TypeCheckFailed (Module SourceRegion Frontend) ElaraError
    | -- | The file was parsed and type checked
      Checked (Module SourceRegion Frontend) (Module SourceRegion Typed)
    | -- | Something went wrong outside of any stage, i.e. an error that somehow escaped normal handling, or the compiler crashing
      CompilerFailed ElaraError

-- | An exception that isn't an 'ElaraError', i.e. the compiler crashing.
newtype CompilerCrash = CompilerCrash SomeException
    deriving (Show)

instance Exception CompilerCrash

instance ElaraDiagnostic CompilerCrash where
    diagnosticMessage (CompilerCrash exception) = "internal compiler error: " <> fromString (show exception)

-- | Like 'tryError', but without the call stack
tryElara :: Error ElaraError :> es => Eff es a -> Eff es (Either ElaraError a)
tryElara = firstF snd . tryError @ElaraError

{- | Run the compiler on a document. Catches all possible errors, including the compiler crashing/panicking,
storing them as 'ElaraError's in the resultant 'CompileResult'
-}
compileDocument :: CompilerSettings -> FilePath -> IO CompileResult
compileDocument settings path = do
    result <-
        tryAny $
            runEff $
                runWriter @[ElaraWarning] $
                    runErrorNoCallStack @ElaraError $
                        runLogAction @_ @(Doc Ann) (LogAction (const pass)) $
                            Elara.withCompilerEnv settings compile
    pure $ case result of
        Left crash -> failed [] (ElaraError (CompilerCrash crash))
        Right (Left escapedElaraError, warnings) -> failed warnings escapedElaraError
        Right (Right compiled, warnings) -> compiled{compiledWarnings = warnings}
  where
    compile = do
        parsed <- tryElara (Rock.fetch (Query.ParsedFile path))
        outcome <- case parsed of
            Left parseError -> pure (ParseFailed parseError)
            Right ast@(Module _ m) -> do
                typed <- tryElara (Rock.fetch (Query.ModuleByName @Typed (m.moduleName ^. unlocated)))
                pure (either (TypeCheckFailed ast) (Checked ast) typed)
        text <- either (const "") fileContents <$> tryElara (Rock.fetch (Query.GetFileContents path))
        pure CompileResult{compiledText = text, compiledWarnings = [], compiledOutcome = outcome}

    failed warnings err = CompileResult{compiledText = "", compiledWarnings = warnings, compiledOutcome = CompilerFailed err}

toSnapshot :: FilePath -> CompileResult -> FileSnapshot
toSnapshot path result =
    FileSnapshot
        { snapshotPath = path
        , snapshotText = compiledText result
        , snapshotReports = dedupeReports (concatMap diagnosticReports (compiledWarnings result) <> foldMap diagnosticReports blocker)
        , snapshotParsed = parsed
        , snapshotTyped = typed
        , snapshotBlocker = blocker
        }
  where
    (parsed, typed, blocker) = case compiledOutcome result of
        ParseFailed err -> (Nothing, Nothing, Just err)
        TypeCheckFailed ast err -> (Just ast, Nothing, Just err)
        Checked ast typedAst -> (Just ast, Just typedAst, Nothing)
        CompilerFailed err -> (Nothing, Nothing, Just err)
