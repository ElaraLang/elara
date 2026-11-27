{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
    main,
)
where

import Autodocodec
import Control.Exception as E
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runError)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Process (Process, runProcess)
import Effectful.Temporary (Temporary, runTemporary)
import Effectful.Writer.Static.Local (Writer, runWriter)
import Error.Diagnose (TabSize (..), WithUnicode (..), printDiagnostic')
import OptEnvConf
import System.CPUTime
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, hSetEncoding, utf8)
import Text.Printf

import Data.Set qualified as Set

import Elara.Data.Pretty
import Elara.Data.Unique (resetGlobalUniqueSupply)
import Elara.Data.Unique.Effect (uniqueGenToGlobalIO)
import Elara.Error
import Elara.Error.Diagnose (diagnoseStyle, reportsToDiagnostic)
import Elara.JVM.Run (runJVMOutput)
import Elara.LSP (runLanguageServer)
import Elara.Pipeline (runLogToStdoutAndFile)
import Elara.Settings (CompilerSettings (..), DumpTarget (..))
import Print (printPretty)
import Prelude hiding (reader)

import Elara qualified
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Stdlib qualified as Stdlib
import Paths_elara qualified as Elara

data Settings = Settings
    { dumpTargets :: [DumpTarget]
    , sourceDirs :: [FilePath]
    , programArgs :: [String]
    , buildDir :: FilePath
    , stdlibDir :: Maybe FilePath
    }

data Instructions = Instructions !Dispatch !Settings

instance HasParser Settings where
    settingsParser = withLocalYamlConfig $ do
        dumpTargets <-
            setting
                [ help "Dump intermediate ASTs/IRs (e.g. --dump=ir,jvm)"
                , name "dump"
                , env "ELARA_DUMP"
                , metavar "DUMP_TARGETS"
                , OptEnvConf.value []
                , reader $ commaSeparatedList viaStringCodec
                ]
        sourceDirs <-
            setting
                [ help "Source directories"
                , name "source-dirs"
                , env "ELARA_SOURCE_DIRS"
                , metavar "SOURCE_DIRS"
                , OptEnvConf.value []
                , reader $ commaSeparatedList str
                ]
        programArgs <-
            many
                (setting [argument, help "Arguments to pass to the program", metavar "PROGRAM_ARGS", reader str])

        buildDir <-
            setting
                [ help "Output directory for compiled files (default: build/)"
                , name "output-dir"
                , env "ELARA_OUTPUT_DIR"
                , metavar "OUTPUT_DIR"
                , reader str
                , value "build"
                ]
        stdlibDir <-
            optional $
                setting
                    [ help "Directory containing the standard library (default: the stdlib bundled with elara)"
                    , name "stdlib-dir"
                    , env "ELARA_STDLIB_DIR"
                    , metavar "STDLIB_DIR"
                    , reader str
                    ]
        pure Settings{..}

instance HasParser Instructions where
    settingsParser = Instructions <$> settingsParser <*> settingsParser

instance HasCodec DumpTarget where
    codec =
        stringConstCodec $
            (DumpLexed, "lexed")
                :| [ (DumpParsed, "parsed")
                   , (DumpDesugared, "desugared")
                   , (DumpRenamed, "renamed")
                   , (DumpShunted, "shunted")
                   , (DumpTyped, "typed")
                   , (DumpCore, "core")
                   , (DumpIR, "ir")
                   , (DumpJVM, "jvm")
                   , (DumpQueryGraph, "query-graph")
                   ]

data Dispatch
    = DispatchBuild !FilePath !RunTarget
    | DispatchRun !FilePath !RunTarget
    | DispatchStdlibPath
    | DispatchStdlibEject !FilePath
    | DispatchLsp
    deriving (Eq, Show)

data RunTarget = TargetInterpreter | TargetJVM
    deriving (Eq, Generic, Show)

targetParser :: Parser RunTarget
targetParser =
    setting
        [ help "Execution target (interp, jvm)"
        , option
        , name "target"
        , metavar "EXECUTION_TARGET"
        , value TargetInterpreter
        , reader viaStringCodec
        ]

instance HasParser Dispatch where
    settingsParser =
        withoutConfig $
            commands
                [ command "build" "Compile the program" $
                    DispatchBuild
                        <$> setting
                            [ help "Main file to compile"
                            , argument
                            , reader str
                            , metavar "MAIN_FILE"
                            ]
                        <*> targetParser
                , command "run" "Compile and run the program" $
                    DispatchRun
                        <$> setting
                            [ help "Main file to run"
                            , argument
                            , reader str
                            , metavar "MAIN_FILE"
                            ]
                        <*> targetParser
                , command "lsp" "Start the Elara Language Server" $
                    DispatchLsp
                        <$ setting
                            [ help "Mostly useful for editor compatibility"
                            , switch True
                            , long "stdio"
                            , value False
                            ]
                , command "stdlib" "Inspect or customise the standard library" $
                    commands
                        [ command "path" "Print the directory the standard library is loaded from" $
                            pure DispatchStdlibPath
                        , command "eject" "Copy the bundled standard library into a directory, for use with --stdlib-dir" $
                            DispatchStdlibEject
                                <$> setting
                                    [ help "Directory to write the standard library to"
                                    , argument
                                    , reader str
                                    , metavar "DIR"
                                    , value "stdlib"
                                    ]
                        ]
                ]

instance HasCodec RunTarget where
    codec = stringConstCodec $ (TargetInterpreter, "interp") :| [(TargetJVM, "jvm")]

toCompilerSettings :: Maybe FilePath -> FilePath -> Settings -> CompilerSettings
toCompilerSettings mainFile resolvedStdlibDir Settings{..} =
    CompilerSettings
        { dumpTargets = Set.fromList dumpTargets
        , mainFile = mainFile
        , sourceDirs = sourceDirs
        , programArgs = programArgs
        , outputDir = buildDir
        , stdlibDir = resolvedStdlibDir
        }

targetToBackend :: RunTarget -> Elara.Backend
targetToBackend TargetInterpreter = Elara.Interpreter
targetToBackend TargetJVM = Elara.JVM

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    Instructions dispatch settings <-
        runSettingsParser Elara.version "Elara Compiler"

    case dispatch of
        DispatchBuild mainFile target -> compile mainFile (Elara.CompileAndEmit (targetToBackend target)) settings
        DispatchRun mainFile target -> compile mainFile (Elara.CompileAndRun (targetToBackend target)) settings
        DispatchLsp -> do
            -- we don't use 'runReporting' here since that prints to stdout, which would confuse the LSP client
            resolvedStdlib <- runEff . runTemporary . runFileSystem . runError @ElaraError $ Stdlib.resolveStdlibDir settings.stdlibDir
            case resolvedStdlib of
                Left (_, err) -> do
                    hPutStrLn stderr ("elara lsp: could not find the standard library: " <> toString (prettyToUnannotatedText err))
                    exitFailure
                Right stdlibDir -> do
                    code <- runLanguageServer (toCompilerSettings Nothing stdlibDir settings)
                    exitWith (if code == 0 then ExitSuccess else ExitFailure code)
        DispatchStdlibPath -> void $ runReporting $ do
            dir <- Stdlib.resolveStdlibDir settings.stdlibDir
            liftIO $ putStrLn dir
        DispatchStdlibEject dir -> void $ runReporting $ do
            Stdlib.ejectStdlib dir
            liftIO $
                printPretty $
                    "Wrote the standard library to"
                        <+> Style.bold (pretty dir)
                        <> ". To use it, pass"
                            <+> Style.bold ("--stdlib-dir" <+> pretty dir)

compile :: FilePath -> Elara.CompileAction -> Settings -> IO ()
compile mainFile action settings = run `finally` cleanup
  where
    run = do
        start <- getCPUTime
        mResult <- runReporting $ do
            stdlibDir <- Stdlib.resolveStdlibDir settings.stdlibDir
            let compilerSettings = toCompilerSettings (Just mainFile) stdlibDir settings
            result <- uniqueGenToGlobalIO $ runLogToStdoutAndFile $ Elara.compile compilerSettings action
            when (action == Elara.CompileAndRun Elara.JVM) $
                runJVMOutput compilerSettings.outputDir
            pure result

        whenJust mResult $ \result -> do
            end <- getCPUTime
            let t :: Double
                t = fromIntegral (end - start) * 1e-9
                verb = case action of
                    Elara.CompileOnly -> "compiled"
                    Elara.CompileAndEmit _ -> "compiled"
                    Elara.CompileAndRun _ -> "ran"
            printPretty
                ( Style.varName "Successfully"
                    <+> verb
                    <+> Style.punctuation (pretty (length result.sourceFiles))
                    <> " source files in "
                    <> Style.punctuation (fromString (printf "%.2f" t) <> "ms!")
                )

{- | Run an action with the compiler's error and warning handling, printing any diagnostics it produces.
Returns 'Nothing' if it failed with an error.
-}
runReporting :: Eff '[Error ElaraError, Writer [ElaraWarning], FileSystem, Temporary, Process, IOE] a -> IO (Maybe a)
runReporting action = do
    (reports, mResult) <- runEff $ runProcess $ runTemporary $ runFileSystem $ do
        (elaraResult, warnings) <- runWriter @[ElaraWarning] $ runError @ElaraError action

        let warnReports = concatMap diagnosticReports warnings

        case elaraResult of
            Left (callStack, err) -> do
                liftIO $ printPretty callStack
                pure (warnReports <> diagnosticReports err, Nothing)
            Right r -> pure (warnReports, Just r)

    unless (null reports) $ do
        diag <- reportsToDiagnostic (dedupeReports reports)
        printDiagnostic' stdout WithUnicode (TabSize 4) diagnoseStyle diag

    pure mResult

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply
