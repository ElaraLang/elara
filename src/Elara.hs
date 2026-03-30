{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level compiler library entrypoint.
module Elara (
    -- * Action types
    Backend (..),
    CompileAction (..),

    -- * Result types
    CompileResult (..),

    -- * High-level entrypoint
    compile,

    -- * Utilities for working with the compiler environment
    withCompilerEnv,
    resolveModules,
    dumpStages,
    emitJVM,
    runInterpreter,
)
where

import Data.Dependent.HashMap qualified as DHashMap
import Data.Generics.Product (field')
import Data.Set qualified as Set
import Effectful (Eff, IOE, Subset, (:>))
import Effectful.Colog
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Elara.AST.Instances ()
import Elara.AST.Module qualified as New
import Elara.AST.Name (ModuleName, NameLike, nameText)
import Elara.AST.Phase (Locate)
import Elara.AST.Phases.Shunted qualified as NewS
import Elara.AST.Region
import Elara.Core.LiftClosures.Error (ClosureLiftError)
import Elara.Core.Module (CoreModule)
import Elara.Data.Pretty
import Elara.Data.Unique.Effect
import Elara.Desugar.Error (DesugarError)
import Elara.Error
import Elara.Interpreter qualified as Interpreter
import Elara.JVM.Error (JVMLoweringError)
import Elara.JVM.IR qualified as IR
import Elara.Lexer.Utils (LexerError)
import Elara.Logging (LogConfig (..), LogLevel (Info), StructuredDebug, getLogConfigFromEnv, ignoreStructuredDebug, logDebug, structuredDebugToLogWith)
import Elara.Parse.Error (WParseErrorBundle)
import Elara.Query qualified
import Elara.ReadFile (ModulePathError)
import Elara.Rename.Error (RenameError)
import Elara.Rules qualified
import Elara.Settings (CompilerSettings (..), DumpTarget (..))
import Elara.Shunt.Error (ShuntError)
import Error.Diagnose (Report (..))
import JVM.Data.Abstract.ClassFile (ClassFile (..))
import JVM.Data.Convert.Monad (CodeConverterError)
import Prettyprinter.Render.Text
import Print
import Rock qualified
import Rock.Memo qualified
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, takeDirectory)
import System.IO (hSetEncoding, utf8)
import Prelude hiding (reader)

-- | Backend target for code generation and execution
data Backend = Interpreter | JVM
    deriving (Show, Eq)

-- | What action to take after compilation
data CompileAction
    = -- | Just compile and do nothing else (e.g. for type-checking or dumping intermediate stages).
      CompileOnly
    | -- | Compile and emit artifacts (e.g. emit JVM .class files), but do not execute.
      CompileAndEmit !Backend
    | -- | Compile and execute the emitted code.
      CompileAndRun !Backend
    deriving (Show, Eq)

-- | Orphan instance, TODO: sort this out by improving h2jvm
instance ReportableError CodeConverterError where
    report x =
        writeReport $
            Err
                Nothing
                (show x)
                []
                []

-- | Result of compilation, containing information useful to consumers.
data CompileResult = CompileResult
    { moduleNames :: [ModuleName]
    -- ^ Names of all compiled modules
    , sourceFiles :: [FilePath]
    -- ^ Source files that were compiled
    , emittedFiles :: [FilePath]
    -- ^ Files emitted to disk (e.g. JVM .class files). Empty if no emission was done or if the backend doesn't emit files. Does not include dumped intermediate representations.
    }
    deriving (Show)

-- | Top-level compiler entrypoint. Returns a 'CompileResult' describing what was compiled.
compile ::
    ( Error SomeReportableError :> es
    , DiagnosticWriter (Doc AnsiStyle) :> es
    , Log (Doc AnsiStyle) :> es
    , UniqueGen :> es
    , IOE :> es
    ) =>
    -- | Settings to compile with
    CompilerSettings ->
    -- | What action to take after compilation
    CompileAction ->
    Eff es CompileResult
compile settings action = withCompilerEnv settings $ do
    liftIO (createDirectoryIfMissing True settings.outputDir)

    resolved <- resolveModules
    let (modNames, files) = unzip resolved
    dumpStages settings resolved

    emitted <- case action of
        CompileOnly -> pure []
        CompileAndEmit JVM -> emitJVM settings modNames
        CompileAndEmit Interpreter -> pure []
        CompileAndRun Interpreter -> do
            runInterpreter
            pure []
        CompileAndRun JVM -> emitJVM settings modNames

    pure CompileResult{moduleNames = modNames, sourceFiles = files, emittedFiles = emitted}

{- | Set up the compiler effect stack, then runs the given action inside it.
Note that this contains only local state, so things like memoisation will not persist across multiple calls to 'withCompilerEnv'.
-}
withCompilerEnv ::
    ( Error SomeReportableError :> es
    , DiagnosticWriter (Doc AnsiStyle) :> es
    , IOE :> es
    , Log (Doc AnsiStyle) :> es
    , HasCallStack
    ) =>
    -- | The settings to run the compiler with
    CompilerSettings ->
    -- | The action to run inside the compiler environment
    Eff _ a ->
    -- | The result of the action, lifted into the effect stack
    Eff es a
withCompilerEnv settings action = do
    logConfig <- liftIO getLogConfigFromEnv
    let shouldEnableLogging = elaraDebug || minLogLevel logConfig <= Info
    startedVar <- liftIO $ newIORef DHashMap.empty
    depsVar <- liftIO $ newIORef mempty
    runFileSystem $
        uniqueGenToGlobalIO $
            (if shouldEnableLogging then structuredDebugToLogWith logConfig else ignoreStructuredDebug) $
                runConcurrent $
                    runErrorOrReport @ModulePathError $
                        Rock.runRock
                            (Rock.Memo.memoiseWithCycleDetection startedVar depsVar (Elara.Rules.rules settings))
                            action

{- | Discover input files and resolve module names.
Returns a list of (module name, source file path) pairs.
-}
resolveModules ::
    ( Rock.Rock Elara.Query.Query :> es
    , Error SomeReportableError :> es
    , DiagnosticWriter (Doc AnsiStyle) :> es
    , StructuredDebug :> es
    , Concurrent :> es
    , FileSystem :> es
    , UniqueGen :> es
    , IOE :> es
    , HasCallStack
    ) =>
    -- | (Module names, source files)
    Eff es [(ModuleName, FilePath)]
resolveModules = do
    files <- toList <$> Rock.fetch Elara.Query.InputFiles
    modNames <- for files $ \file -> do
        (New.Module _ m) <-
            runErrorOrReport @(WParseErrorBundle _ _) $
                Rock.fetch $
                    Elara.Query.ParsedFile file
        pure (New.moduleName m ^. unlocated)
    pure (zip modNames files)

-- | Dump whichever stages are enabled in 'CompilerSettings'.
dumpStages ::
    forall es.
    ( Rock.Rock Elara.Query.Query :> es
    , Error SomeReportableError :> es
    , DiagnosticWriter (Doc AnsiStyle) :> es
    , StructuredDebug :> es
    , Concurrent :> es
    , FileSystem :> es
    , UniqueGen :> es
    , IOE :> es
    , HasCallStack
    ) =>
    -- | The compiler settings
    CompilerSettings ->
    -- | The resolved modules as (module name, source file path) pairs
    [(ModuleName, FilePath)] ->
    Eff es ()
dumpStages settings resolved = do
    let moduleNames = map fst resolved
        files = map snd resolved
        outDir = settings.outputDir

    when (DumpLexed `Set.member` dumpTargets) $ do
        lexed <- for files $ \file -> do
            fmap (file,) $ runErrorOrReport @LexerError $ Rock.fetch $ Elara.Query.LexedFile file
        dumpGraph outDir lexed (toText . takeBaseName . fst) ".lexed.elr"
        logDebug "Dumped lexed files"

    let dumpGraphInfo' ::
            forall error module' xs.
            (ReportableError error, Subset xs (Error error : es), Pretty module', StructuredDebug :> xs) =>
            (module' -> Text) -> (ModuleName -> Elara.Query.Query xs module') -> DumpTarget -> Text -> Text -> Eff es ()
        dumpGraphInfo' nameFunc query target name suffix =
            runErrorOrReport @error $
                dumpGraphInfo outDir nameFunc query (target `Set.member` dumpTargets) moduleNames name suffix

        coreNameFunc :: CoreModule bind -> Text
        coreNameFunc m = m ^. field' @"name" % to nameText

        newModuleNameFunc :: NameLike (Locate loc ModuleName) => New.Module loc p -> Text
        newModuleNameFunc (New.Module _ m) = New.moduleName m ^. to nameText

    dumpGraphInfo' @(WParseErrorBundle _ _) newModuleNameFunc Elara.Query.ParsedModule DumpParsed "parsed" ".parsed.elr"
    dumpGraphInfo' @DesugarError newModuleNameFunc Elara.Query.DesugaredModule DumpDesugared "desugared" ".desugared.elr"
    dumpGraphInfo' @RenameError newModuleNameFunc Elara.Query.RenamedModule DumpRenamed "renamed" ".renamed.elr"
    dumpGraphInfo' @ShuntError newModuleNameFunc (Elara.Query.ModuleByName @NewS.Shunted) DumpShunted "shunted" ".shunted.elr"
    dumpGraphInfo' @ShuntError newModuleNameFunc Elara.Query.TypeCheckedModule DumpTyped "typed" ".typed.elr"
    dumpGraphInfo' @ShuntError coreNameFunc Elara.Query.GetCoreModule DumpCore "core" ".core.elr"
    dumpGraphInfo' @ShuntError coreNameFunc Elara.Query.GetANFCoreModule DumpCore "core.anf" ".core.anf.elr"
    dumpGraphInfo' @ClosureLiftError coreNameFunc Elara.Query.GetClosureLiftedModule DumpCore "core.closure_lifted" ".core.closure_lifted.elr"
    dumpGraphInfo outDir coreNameFunc Elara.Query.GetFinalisedCoreModule (DumpCore `Set.member` dumpTargets) moduleNames "core.final" ".core.final.elr"

    when (DumpIR `Set.member` dumpTargets) $ do
        irModules <- for moduleNames $ \m ->
            runErrorOrReport @JVMLoweringError $ Rock.fetch $ Elara.Query.GetJVMIRModule m
        dumpGraph outDir irModules (\x -> prettyToUnannotatedText x.moduleName) ".jvm.ir.elr"
        logDebug "Dumped JVM IR modules"

    when (DumpJVM `Set.member` dumpTargets) $ do
        classFiles <- for moduleNames $ \m ->
            runErrorOrReport @JVMLoweringError $ Rock.fetch $ Elara.Query.GetJVMClassFiles m
        dumpGraph outDir (concat classFiles) (\(cf :: ClassFile) -> prettyToUnannotatedText cf.name) ".classfile.txt"
  where
    CompilerSettings{dumpTargets} = settings

-- | Emit JVM .class files to disk for all modules. Returns emitted file paths.
emitJVM ::
    ( Rock.Rock Elara.Query.Query :> es
    , Error SomeReportableError :> es
    , DiagnosticWriter (Doc AnsiStyle) :> es
    , StructuredDebug :> es
    , Concurrent :> es
    , FileSystem :> es
    , UniqueGen :> es
    , IOE :> es
    , HasCallStack
    ) =>
    -- | Compiler settings (for output directory)
    CompilerSettings ->
    -- | Modules to emit
    [ModuleName] ->
    Eff es [FilePath]
emitJVM settings moduleNames = do
    fmap concat $ for moduleNames $ \modName -> do
        classBytes <-
            runErrorOrReport @JVMLoweringError $
                runErrorOrReport @CodeConverterError $
                    Rock.fetch (Elara.Query.GetJVMClassBytes modName)
        for classBytes $ \(fp, bytes) -> do
            let fullPath = settings.outputDir <> "/" <> fp
            liftIO $ createAndWriteFile fullPath bytes
            pure fullPath

-- | Run the interpreter on the compiled modules (prints output to stdout).
runInterpreter ::
    ( Rock.Rock Elara.Query.Query :> es
    , Error SomeReportableError :> es
    , DiagnosticWriter (Doc AnsiStyle) :> es
    , StructuredDebug :> es
    , Concurrent :> es
    , FileSystem :> es
    , UniqueGen :> es
    , IOE :> es
    ) =>
    Eff es ()
runInterpreter =
    Interpreter.runInterpreterOutput $
        Interpreter.runInterpreter Interpreter.run

-- | Dump a graph of pretty-printable items to disk, one file per item. The file name is determined by the provided name function and suffix.
dumpGraph ::
    (HasCallStack, Pretty m, Foldable f, IOE :> es) =>
    -- | Output directory
    FilePath ->
    -- | The graph to dump
    f m ->
    -- | Function to determine the name of each dumped item (e.g. module name for modules)
    (m -> Text) ->
    -- | Suffix to append to each file name (e.g. ".core.elr")
    Text ->
    Eff es ()
dumpGraph outDir graph nameFunc suffix = do
    let dump m = do
            let contents = pretty m
            let fileName = toString (toText outDir <> "/" <> nameFunc m <> suffix)
            let rendered = layoutSmart defaultLayoutOptions contents
            withFile fileName WriteMode $ \fileHandle -> do
                hSetEncoding fileHandle utf8
                renderIO fileHandle rendered
                hFlush fileHandle

    traverse_ (liftIO . dump) graph

{- | Dump a graph of pretty-printable items to disk for a specific compiler stage, if that stage is enabled in the settings.
This essentially takes a list of module names, queries them all, then dumps all the results.
-}
dumpGraphInfo ::
    ( Subset xs es
    , Rock.Rock Elara.Query.Query :> es
    , HasCallStack
    , StructuredDebug :> es
    , IOE :> es
    , Pretty module'
    , StructuredDebug :> xs
    ) =>
    -- | Output directory
    FilePath ->
    -- | Function to determine the name of each dumped item (e.g. module name for modules)
    (module' -> Text) ->
    -- | Function to query for each item to dump
    (ModuleName -> Elara.Query.Query xs module') ->
    -- | Whether to dump. If this is 'False', this function does nothing.
    Bool ->
    -- | List of all module names to dump
    [ModuleName] ->
    -- | Name of the stage being dumped (for logging)
    Text ->
    -- | Suffix to append to each file name (e.g. ".core.elr")
    Text ->
    Eff es ()
dumpGraphInfo outDir nameFunc query when' moduleNames stage suffix = do
    when when' $ do
        modules <- for moduleNames $ \m -> do
            Rock.fetch $ query m
        dumpGraph outDir modules nameFunc suffix
        logDebug ("Dumped " <> pretty (length modules) <> " " <> pretty stage <> " modules")

createAndWriteFile :: FilePath -> LByteString -> IO ()
createAndWriteFile path content = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFileLBS path content
