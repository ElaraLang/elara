{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
    main,
)
where

import Control.Exception as E
import Elara.AST.Module
import Elara.AST.Region (Located (..), unlocated)

-- import Elara.CoreToIR

import Effectful (Eff, IOE, runEff, (:>))
import Effectful.FileSystem (runFileSystem)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraph_)
import Elara.Data.Unique (
    resetGlobalUniqueSupply,
 )

import Effectful.Colog
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (Error, runError)
import Elara.Data.Unique.Effect
import Elara.Error
import Elara.Logging (structuredDebugToLog)
import Elara.Parse.Error (WParseErrorBundle (WParseErrorBundle))
import Elara.Pipeline (runLogToStdoutAndFile)
import Elara.Query qualified
import Elara.Rules qualified
import Elara.Settings (CompilerSettings (CompilerSettings, dumpSettings), DumpSettings (..))
import Error.Diagnose (Report (..), TabSize (..), WithUnicode (..), defaultStyle, printDiagnostic')
import JVM.Data.Convert.Monad
import Prettyprinter.Render.Text
import Print
import Rock qualified
import Rock.Memo (MemoQuery)
import Rock.Memo qualified
import Rock.MemoE (memoiseAsIO, memoiseRunIO)
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
                CompilerSettings{dumpSettings = DumpSettings{..}}
        (diagnostics, ()) <- runEff $ runDiagnosticWriter $ do
            result <- runError @SomeReportableError $ runElaraWrapped compilerSettings
            case result of
                Left (callStack, error) -> do
                    report error
                    pure mempty
                Right ok -> pass

        printDiagnostic' stdout WithUnicode (TabSize 4) defaultStyle diagnostics

dumpGraph :: (HasCallStack, Pretty m) => TopologicalGraph m -> (m -> Text) -> Text -> IO ()
dumpGraph graph nameFunc suffix = do
    let dump m = do
            let contents = pretty m
            let fileName = toString (outDirName <> "/" <> nameFunc m <> suffix)
            let rendered = layoutSmart defaultLayoutOptions contents
            withFile fileName WriteMode $ \fileHandle -> do
                hSetEncoding fileHandle utf8
                renderIO fileHandle rendered
                hFlush fileHandle

    traverseGraph_ dump graph

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

                                loadedModules <- for ["source.elr"] $ \file -> do
                                    (Module (Located _ m)) <- runErrorOrReport @(WParseErrorBundle _ _) $ Rock.fetch $ Elara.Query.ParsedFile file
                                    Rock.fetch $ Elara.Query.TypeCheckedModule (m.name ^. unlocated)

                                printPretty loadedModules
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

-- when run $ liftIO $ do
--     printPretty (Style.varName "Running code...")
--     -- run 'java -cp ../jvm-stdlib:. Main' in pwd = './build'
--     let process =
--             if os == "mingw32"
--                 then shell "java -noverify -cp ../jvm-stdlib;. Main"
--                 else shell "java -noverify -cp ../jvm-stdlib:. Main"
--     x <- readCreateProcess process{cwd = Just "./build"} ""
--     putStrLn x

-- createAndWriteFile :: FilePath -> LByteString -> IO ()
-- createAndWriteFile path content = do
--     createDirectoryIfMissing True $ takeDirectory path
--     writeFileLBS path content

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply

-- processModules ::
--     IsPipeline r =>
--     TopologicalGraph (Module 'Desugared) ->
--     (Bool, Bool) ->
--     Sem r (TopologicalGraph (CoreModule CoreBind))
-- processModules graph (dumpShunted, dumpTyped) =
--     runToCorePipeline $
--         runInferPipeline $
--             runShuntPipeline $
--                 runRenamePipeline
--                     graph
--                     primitiveRenameState
--                     ( traverseGraph rename
--                         >=> shuntGraph (Just primOpTable)
--                         >=> dumpIf identity dumpShunted (view (_Unwrapped % unlocated % field' @"name" % to nameText)) ".shunted.elr"
--                         >=> traverseGraphRevTopologically inferModule
--                         >=> dumpIf identity dumpTyped (view (_Unwrapped % unlocated % field' @"name" % to nameText)) ".typed.elr"
--                         >=> traverseGraphRevTopologically moduleToCore
--                         >=> (pure . mapGraph coreToCore)
--                         $ graph
--                     )
--   where
--     dumpIf acc cond f p = if cond then (\x -> liftIO (dumpGraph (mapGraph acc x) f p) $> x) else pure
