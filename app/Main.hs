{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (
    main,
)
where

import Control.Exception as E
import Effectful (Eff, IOE, Subset, inject, runEff, (:>))
import Effectful.FileSystem (runFileSystem)
import Elara.AST.Select
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.Unique (
    resetGlobalUniqueSupply,
 )

import Autodocodec
import Data.Binary.Put
import Data.Binary.Write
import Data.Generics.Product (field')
import Data.Generics.Wrapped (_Unwrapped)
import Data.Set qualified as Set
import Effectful.Colog
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Extra (fromEither)
import Effectful.Error.Static (Error, runError)
import Elara.AST.Generic
import Elara.AST.Module
import Elara.AST.Name (ModuleName, NameLike, nameText)
import Elara.AST.Region
import Elara.Core.LiftClosures.Error (ClosureLiftError)
import Elara.Core.Module (CoreModule)
import Elara.Data.Unique.Effect
import Elara.Desugar.Error (DesugarError)
import Elara.Error
import Elara.Interpreter qualified as Interpreter
import Elara.JVM.Emit qualified as Emit
import Elara.JVM.Error (JVMLoweringError)
import Elara.JVM.IR (moduleName)
import Elara.JVM.Lower
import Elara.Lexer.Utils (LexerError)
import Elara.Logging (LogConfig (..), LogLevel (Info), StructuredDebug, getLogConfigFromEnv, ignoreStructuredDebug, logDebug, logInfo, logWarning, minLogLevel, structuredDebugToLogWith)
import Elara.Parse.Error (WParseErrorBundle)
import Elara.Pipeline (runLogToStdoutAndFile)
import Elara.Query qualified
import Elara.Rename.Error (RenameError)
import Elara.Rules qualified
import Elara.Settings (CompilerSettings (..), DumpTarget (..), RunWithOption (..))
import Elara.Shunt.Error (ShuntError)
import Error.Diagnose (Report (..), TabSize (..), WithUnicode (..), defaultStyle, printDiagnostic')
import JVM.Data.Abstract.ClassFile
import JVM.Data.Abstract.Name
import JVM.Data.Convert (convert)
import JVM.Data.Convert.Monad
import OptEnvConf
import Paths_elara qualified as Elara
import Prettyprinter.Render.Text
import Print
import Rock qualified
import Rock.Memo qualified
import Rock.MemoE (memoiseRunIO)
import System.CPUTime
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, takeDirectory)
import System.IO (hSetEncoding, utf8)
import Text.Printf
import Prelude hiding (reader)

outDirName :: IsString s => s
outDirName = "build"

instance ReportableError CodeConverterError where
    report x =
        writeReport $
            Err
                Nothing
                (show x)
                []
                []

newtype Settings = Settings
    { dumpTargets :: [DumpTarget]
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
    = DispatchBuild !FilePath
    | DispatchRun !FilePath !RunTarget
    deriving (Show, Eq)

data RunTarget = TargetInterpreter | TargetJVM
    deriving (Show, Eq, Generic)

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
                , command "run" "Compile and run the program" $
                    DispatchRun
                        <$> setting
                            [ help "Main file to run"
                            , argument
                            , reader str
                            , metavar "MAIN_FILE"
                            ]
                        <*> setting
                            [ help "Execution target (interpreter, jvm)"
                            , option
                            , name "target"
                            , metavar "EXECUTION_TARGET"
                            , value TargetInterpreter
                            , reader viaStringCodec
                            ]
                ]

instance HasCodec RunTarget where
    codec = stringConstCodec $ (TargetInterpreter, "interp") :| [(TargetJVM, "jvm")]

toCompilerSettings :: Dispatch -> Settings -> CompilerSettings
toCompilerSettings dispatch Settings{..} =
    let
        dumps = Set.fromList dumpTargets

        (runWith, mainFile) = case dispatch of
            DispatchBuild fp -> (RunWithNone, Just fp)
            DispatchRun fp TargetInterpreter -> (RunWithInterpreter, Just fp)
            DispatchRun fp TargetJVM -> (RunWithJVM, Just fp)
     in
        CompilerSettings
            { dumpTargets = dumps
            , runWith = runWith
            , mainFile = mainFile
            }

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8
    Instructions dispatch settings <-
        runSettingsParser Elara.version "Elara Compiler"

    let compilerSettings = toCompilerSettings dispatch settings
    run compilerSettings `finally` cleanup
  where
    run :: CompilerSettings -> IO ()
    run compilerSettings = do
        putTextLn "\n"
        (diagnostics, ()) <- runEff $ runDiagnosticWriter $ do
            result <- runError @SomeReportableError $ runElaraWrapped compilerSettings
            case result of
                Left (callStack, error) -> do
                    report error
                    printPretty callStack
                    pure mempty
                Right () -> printPretty "Done"

        printDiagnostic' stdout WithUnicode (TabSize 4) defaultStyle diagnostics

dumpGraph :: (HasCallStack, Pretty m, Foldable f) => f m -> (m -> Text) -> Text -> Eff '[IOE] ()
dumpGraph graph nameFunc suffix = do
    let dump m = do
            let contents = pretty m
            let fileName = toString (outDirName <> "/" <> nameFunc m <> suffix)
            let rendered = layoutSmart defaultLayoutOptions contents
            withFile fileName WriteMode $ \fileHandle -> do
                hSetEncoding fileHandle utf8
                renderIO fileHandle rendered
                hFlush fileHandle

    traverse_ (liftIO . dump) graph

runElaraWrapped :: (Error SomeReportableError :> es, DiagnosticWriter (Doc AnsiStyle) :> es, IOE :> es) => CompilerSettings -> Eff es ()
runElaraWrapped settings = uniqueGenToGlobalIO $ do
    runLogToStdoutAndFile $ runElara settings

runElara ::
    ( Error SomeReportableError :> es
    , DiagnosticWriter (Doc AnsiStyle) :> es
    , IOE :> es
    , Log (Doc AnsiStyle) :> es
    , HasCallStack
    ) =>
    CompilerSettings -> Eff es ()
runElara settings@(CompilerSettings{dumpTargets, runWith}) = do
    -- Get logging configuration from environment variables
    logConfig <- liftIO getLogConfigFromEnv
    let shouldEnableLogging = elaraDebug || minLogLevel logConfig <= Info
    runFileSystem $
        uniqueGenToGlobalIO $
            (if shouldEnableLogging then structuredDebugToLogWith logConfig else ignoreStructuredDebug) $
                runConcurrent $
                    memoiseRunIO @Elara.Query.Query $
                        Rock.runRock (Rock.Memo.memoise (Elara.Rules.rules settings)) $
                            do
                                start <- liftIO getCPUTime
                                liftIO (createDirectoryIfMissing True outDirName)

                                files <-
                                    toList <$> Rock.fetch Elara.Query.InputFiles

                                when (DumpLexed `Set.member` dumpTargets) $ do
                                    lexed <- for files $ \file -> do
                                        fmap (file,) $ runErrorOrReport @LexerError $ Rock.fetch $ Elara.Query.LexedFile file

                                    inject $ dumpGraph lexed (toText . takeBaseName . fst) ".lexed.elr"
                                    logDebug "Dumped lexed files"

                                moduleNames <- for files $ \file -> do
                                    (Module (Located _ m)) <- runErrorOrReport @(WParseErrorBundle _ _) $ Rock.fetch $ Elara.Query.ParsedFile file
                                    pure (m.name ^. unlocated)

                                runErrorOrReport @(WParseErrorBundle _ _) $
                                    dumpGraphInfo Elara.Query.ParsedModule (DumpParsed `Set.member` dumpTargets) moduleNames "parsed" ".parsed.elr"

                                runErrorOrReport @DesugarError $
                                    dumpGraphInfo Elara.Query.DesugaredModule (DumpDesugared `Set.member` dumpTargets) moduleNames "desugared" ".desugared.elr"

                                runErrorOrReport @RenameError $
                                    dumpGraphInfo Elara.Query.RenamedModule (DumpRenamed `Set.member` dumpTargets) moduleNames "renamed" ".renamed.elr"

                                runErrorOrReport @ShuntError $
                                    dumpGraphInfo (Elara.Query.ModuleByName @Shunted) (DumpShunted `Set.member` dumpTargets) moduleNames "shunted" ".shunted.elr"

                                runErrorOrReport @ShuntError $
                                    dumpGraphInfo Elara.Query.TypeCheckedModule (DumpTyped `Set.member` dumpTargets) moduleNames "typed" ".typed.elr"
                                runErrorOrReport @ShuntError $
                                    dumpGraphInfo Elara.Query.GetCoreModule (DumpCore `Set.member` dumpTargets) moduleNames "core" ".core.elr"

                                runErrorOrReport @ShuntError $
                                    dumpGraphInfo Elara.Query.GetANFCoreModule (DumpCore `Set.member` dumpTargets) moduleNames "core.anf" ".core.anf.elr"

                                runErrorOrReport @ClosureLiftError $
                                    dumpGraphInfo Elara.Query.GetClosureLiftedModule (DumpCore `Set.member` dumpTargets) moduleNames "core.closure_lifted" ".core.closure_lifted.elr"

                                dumpGraphInfo Elara.Query.GetFinalisedCoreModule (DumpCore `Set.member` dumpTargets) moduleNames "core.final" ".core.final.elr"

                                if runWith == RunWithInterpreter
                                    then
                                        Interpreter.runInterpreter Interpreter.run
                                    else
                                        if runWith == RunWithJVM
                                            then do
                                                cores <- for moduleNames $ \m -> do
                                                    Rock.fetch $ Elara.Query.GetFinalisedCoreModule m
                                                classFiles <- for cores $ \coreMod -> do
                                                    lowered <- runErrorOrReport @JVMLoweringError $ lowerModule coreMod
                                                    Emit.emitIRModule lowered

                                                when (DumpIR `Set.member` dumpTargets) $ do
                                                    lowered <- for cores $ \coreMod -> do
                                                        runErrorOrReport @JVMLoweringError $ lowerModule coreMod
                                                    inject $ dumpGraph lowered (\x -> prettyToUnannotatedText x.moduleName) ".jvm.ir.elr"
                                                    logDebug "Dumped JVM IR modules"

                                                when (DumpJVM `Set.member` dumpTargets) $ do
                                                    inject $ dumpGraph (concat classFiles) (\x -> prettyToUnannotatedText x.name) ".classfile.txt"

                                                for_ (zip moduleNames classFiles) $ \(modName, classes) -> do
                                                    fps <- for classes $ \classFile -> do
                                                        x <- runErrorOrReport $ fromEither $ convert classFile
                                                        let bs = runPut (writeBinary x)
                                                        let fp = "build/" <> suitableFilePath classFile.name
                                                        liftIO $ createAndWriteFile fp bs
                                                        pure fp

                                                    logInfo ("Compiled " <> pretty modName <> " to " <> pretty fps <> "!")

                                                logDebug "Emitted JVM class files"
                                            else logWarning "Nothing to do. Use --run or --run-jvm to execute the compiled code."

                                end <- liftIO getCPUTime
                                let t :: Double
                                    t = fromIntegral (end - start) * 1e-9
                                logInfo
                                    ( Style.varName "Successfully" <+> "compiled "
                                        <> Style.punctuation (pretty (length files))
                                        <> " classes in "
                                        <> Style.punctuation
                                            ( fromString (printf "%.2f" t)
                                                <> "ms!"
                                            )
                                    )

dumpGraphInfo ::
    ( Subset xs es
    , Rock.Rock Elara.Query.Query :> es
    , HasCallStack
    , StructuredDebug :> es
    , IOE :> es
    , Pretty module'
    , Dumpable module'
    ) =>
    (ModuleName -> Elara.Query.Query xs module') -> Bool -> [ModuleName] -> Text -> Text -> Eff es ()
dumpGraphInfo query when' moduleNames stage suffix = do
    when when' $ do
        modules <- for moduleNames $ \m -> do
            Rock.fetch $ query m
        inject $ dumpGraph modules dumpName suffix
        logDebug ("Dumped " <> pretty (length modules) <> " " <> pretty stage <> " modules")

class Dumpable a where
    dumpName :: a -> Text

instance (ASTLocate ast (Module' ast) ~ Located (Module' ast), NameLike (ASTLocate ast ModuleName)) => Dumpable (Module ast) where
    dumpName m = m ^. _Unwrapped % unlocated % field' @"name" % to nameText

instance Dumpable (CoreModule bind) where
    dumpName m = m ^. field' @"name" % to nameText

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply

createAndWriteFile :: FilePath -> LByteString -> IO ()
createAndWriteFile path content = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFileLBS path content
