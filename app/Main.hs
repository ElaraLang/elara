{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
    main,
)
where

import Control.Exception as E

-- import Elara.CoreToIR

import Effectful (Eff, IOE, inject, runEff, (:>))
import Effectful.FileSystem (runFileSystem)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraph_)
import Elara.Data.Unique (
    resetGlobalUniqueSupply,
 )

import Data.Generics.Product (field')
import Data.Generics.Wrapped (_Unwrapped)
import Effectful.Colog
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (Error, runError)
import Elara.AST.Module
import Elara.AST.Name (nameText)
import Elara.AST.Region
import Elara.Core.TypeCheck
import Elara.Data.Unique.Effect
import Elara.Error
import Elara.Interpreter qualified as Interpreter
import Elara.Logging (debug, structuredDebugToLog)
import Elara.Parse.Error (WParseErrorBundle)
import Elara.Pipeline (runLogToStdoutAndFile)
import Elara.Query qualified
import Elara.Rules qualified
import Elara.Settings (CompilerSettings (CompilerSettings, dumpSettings), DumpSettings (..), RunWithOption (..))
import Error.Diagnose (Report (..), TabSize (..), WithUnicode (..), defaultStyle, printDiagnostic')
import JVM.Data.Convert.Monad
import Prettyprinter.Render.Text
import Print
import Rock qualified
import Rock.Memo qualified
import Rock.MemoE (memoiseRunIO)
import System.CPUTime
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.IO (hSetEncoding, utf8)
import Text.Printf

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

main :: IO ()
main = run `finally` cleanup
  where
    run :: IO ()
    run = do
        hSetBuffering stdout NoBuffering
        hSetEncoding stdout utf8
        putTextLn "\n"
        args <- getArgs
        env <- getEnvironment
        let dumpLexed = "--dump-lexed" `elem` args || "ELARA_DUMP_LEXED" `elem` fmap fst env
        let dumpParsed = "--dump-parsed" `elem` args || "ELARA_DUMP_PARSED" `elem` fmap fst env
        let dumpDesugared = "--dump-desugared" `elem` args || "ELARA_DUMP_DESUGARED" `elem` fmap fst env
        let dumpShunted = "--dump-shunted" `elem` args || "ELARA_DUMP_SHUNTED" `elem` fmap fst env
        let dumpTyped = "--dump-typed" `elem` args || "ELARA_DUMP_TYPED" `elem` fmap fst env
        let dumpCore = "--dump-core" `elem` args || "ELARA_DUMP_CORE" `elem` fmap fst env
        let run = "--run" `elem` args || "ELARA_RUN" `elem` fmap fst env

        let compilerSettings =
                CompilerSettings{dumpSettings = DumpSettings{runWith = if run then RunWithInterpreter else RunWithNone, ..}}
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
    ) =>
    CompilerSettings -> Eff es ()
runElara settings@(CompilerSettings{dumpSettings = DumpSettings{..}}) = do
    runFileSystem $
        uniqueGenToGlobalIO $
            structuredDebugToLog $
                runConcurrent $
                    memoiseRunIO @Elara.Query.Query $
                        Rock.runRock (Rock.Memo.memoise (Elara.Rules.rules settings)) $
                            do
                                start <- liftIO getCPUTime
                                liftIO (createDirectoryIfMissing True outDirName)

                                files <-
                                    toList <$> Rock.fetch Elara.Query.InputFiles

                                moduleNames <- for files $ \file -> do
                                    (Module (Located _ m)) <- runErrorOrReport @(WParseErrorBundle _ _) $ Rock.fetch $ Elara.Query.ParsedFile file
                                    pure (m.name ^. unlocated)

                                when dumpTyped $ do
                                    typed <- for moduleNames $ \m -> do
                                        Rock.fetch $ Elara.Query.TypeCheckedModule m
                                    inject $ dumpGraph typed (\x -> x ^. _Unwrapped % unlocated % field' @"name" % to nameText) ".typed.elr"
                                    debug "Dumped typed modules"

                                when dumpCore $ do
                                    core <- for moduleNames $ \m -> do
                                        Rock.fetch $ Elara.Query.GetCoreModule m

                                    inject $ dumpGraph core (\x -> x ^. field' @"name" % to nameText) ".core.elr"

                                -- when dumpCore $ do
                                --     core <- for moduleNames $ \m -> do
                                --         Rock.fetch $ Elara.Query.GetANFCoreModule m
                                --     inject $ dumpGraph core (\x -> x ^. field' @"name" % to nameText ) ".core.anf.elr"

                                -- when dumpCore $ do
                                --     core <- for moduleNames $ \m -> do
                                --         Rock.fetch $ Elara.Query.GetFinalisedCoreModule m
                                --     inject $ dumpGraph core (\x -> x ^. field' @"name" % to nameText ) ".core.final.elr"

                                if runWith == RunWithInterpreter
                                    then
                                        Interpreter.runInterpreter Interpreter.run
                                    else
                                        printPretty (Style.warning "Warning: " <> "nothing to do.")

                                -- printPretty loadedModules
                                end <- liftIO getCPUTime
                                let t :: Double
                                    t = fromIntegral (end - start) * 1e-9
                                printPretty
                                    ( Style.varName "Successfully" <+> "compiled "
                                        <> Style.punctuation (pretty (length files))
                                        <> " classes in "
                                        <> Style.punctuation
                                            ( fromString (printf "%.2f" t)
                                                <> "ms!"
                                            )
                                    )

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply
