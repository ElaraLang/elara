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
import Data.Set qualified as Set
import Effectful (runEff)
import Effectful.Error.Static (runError)
import Elara qualified
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.Unique (resetGlobalUniqueSupply)
import Elara.Data.Unique.Effect (uniqueGenToGlobalIO)
import Elara.Error
import Elara.Pipeline (runLogToStdoutAndFile)
import Elara.Settings (CompilerSettings (..), DumpTarget (..))
import Error.Diagnose (TabSize (..), WithUnicode (..), defaultStyle, printDiagnostic')
import OptEnvConf
import Paths_elara qualified as Elara
import Print (printPretty)
import System.CPUTime
import System.IO (hSetEncoding, utf8)
import System.Process (callProcess)
import Text.Printf
import Prelude hiding (reader)

data Settings = Settings
    { dumpTargets :: [DumpTarget]
    , sourceDirs :: [FilePath]
    , programArgs :: [String]
    , buildDir :: FilePath
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
                   ]

data Dispatch
    = DispatchBuild !FilePath !RunTarget
    | DispatchRun !FilePath !RunTarget
    deriving (Show, Eq)

data RunTarget = TargetInterpreter | TargetJVM
    deriving (Show, Eq, Generic)

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
                ]

instance HasCodec RunTarget where
    codec = stringConstCodec $ (TargetInterpreter, "interp") :| [(TargetJVM, "jvm")]

toCompilerSettings :: Dispatch -> Settings -> CompilerSettings
toCompilerSettings dispatch Settings{..} =
    let
        dumps = Set.fromList dumpTargets
        mainFile = case dispatch of
            DispatchBuild fp _ -> Just fp
            DispatchRun fp _ -> Just fp
     in
        CompilerSettings
            { dumpTargets = dumps
            , mainFile = mainFile
            , sourceDirs = sourceDirs
            , programArgs = programArgs
            , outputDir = buildDir
            }

-- | Convert a CLI dispatch command to a 'Elara.CompileAction'
dispatchToAction :: Dispatch -> Elara.CompileAction
dispatchToAction (DispatchBuild _ target) = Elara.CompileAndEmit (targetToBackend target)
dispatchToAction (DispatchRun _ target) = Elara.CompileAndRun (targetToBackend target)

targetToBackend :: RunTarget -> Elara.Backend
targetToBackend TargetInterpreter = Elara.Interpreter
targetToBackend TargetJVM = Elara.JVM

-- | Execute the JVM output via @java -cp <outDir>:jvm-stdlib Main@.
executeJVM :: CompilerSettings -> IO ()
executeJVM settings =
    callProcess "java" ["-cp", settings.outputDir <> ":jvm-stdlib", "Main"]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    Instructions dispatch settings <-
        runSettingsParser Elara.version "Elara Compiler"

    let compilerSettings = toCompilerSettings dispatch settings
        action = dispatchToAction dispatch
    run action compilerSettings `finally` cleanup
  where
    run :: Elara.CompileAction -> CompilerSettings -> IO ()
    run action compilerSettings = do
        start <- getCPUTime
        (diagnostics, mResult) <- runEff $ runDiagnosticWriter $ do
            result <- runError @SomeReportableError $ uniqueGenToGlobalIO $ runLogToStdoutAndFile $ Elara.compile compilerSettings action
            case result of
                Left (callStack, err) -> do
                    report err
                    printPretty callStack
                    pure Nothing
                Right r -> pure (Just r)

        printDiagnostic' stdout WithUnicode (TabSize 4) defaultStyle diagnostics

        whenJust mResult $ \result -> do
            when (action == Elara.CompileAndRun Elara.JVM) $
                executeJVM compilerSettings

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

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply
