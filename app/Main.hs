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
import Elara.Error.EffectNew qualified as Eff

-- import Elara.CoreToIR

import Effectful (Eff, IOE, runEff, (:>))
import Effectful.FileSystem (runFileSystem)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.TopologicalGraph (TopologicalGraph, traverseGraph_)
import Elara.Data.Unique (
    UniqueSupply,
    freshUniqueSupply,
    resetGlobalUniqueSupply,
 )
import Elara.Error (
    ReportableError (report),
    SomeReportableError (..),
    getReport,
    runErrorOrReportEff,
    writeReport,
 )
import Elara.Prim.Rename (primitiveRenameState)

import Effectful.Concurrent.MVar.Strict (newMVar', runConcurrent)
import Effectful.Error.Static (Error, runError)
import Effectful.State.Static.Local qualified as Local
import Effectful.State.Static.Shared qualified as Shared
import Elara.Data.Unique.Effect
import Elara.Parse.Error (WParseErrorBundle (WParseErrorBundle))
import Elara.Query qualified
import Elara.Rename.Error (RenameError)
import Elara.Rules qualified
import Elara.Settings (CompilerSettings (CompilerSettings, dumpSettings), DumpSettings (..))
import Error.Diagnose (Diagnostic, Report (..), TabSize (..), WithUnicode (..), addReport, defaultStyle, printDiagnostic')
import JVM.Data.Convert.Monad
import Prettyprinter.Render.Text
import Print
import Rock qualified
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
        result <- runEff $ runError @SomeReportableError $ runElaraWrapped compilerSettings
        case result of
            Left (callStack, error) -> do
                putTextLn "\n"
                let diag = getReport error
                case diag of
                    Just d -> printDiagnostic' stdout WithUnicode (TabSize 4) defaultStyle (addReport mempty d)
                    Nothing -> putTextLn "No diagnostic information available."
            Right warnings ->
                printDiagnostic' stdout WithUnicode (TabSize 4) defaultStyle warnings

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

runElaraWrapped :: (Error SomeReportableError :> es, IOE :> es) => CompilerSettings -> Eff es (Diagnostic (Doc AnsiStyle))
runElaraWrapped settings = runConcurrent $ do
    uniqueVars <- newMVar' freshUniqueSupply
    Shared.evalStateMVar uniqueVars $
        runElara settings

runElara :: (Error SomeReportableError :> es, Shared.State UniqueSupply :> es, IOE :> es) => CompilerSettings -> Eff es (Diagnostic (Doc AnsiStyle))
runElara settings@(CompilerSettings{dumpSettings = DumpSettings{..}}) = fmap fst
    <$> runFileSystem
    $ Eff.runDiagnosticWriter
    $ uniqueGenToState
    $ Rock.runRockWith Elara.Rules.rules settings
    $ do
        start <- liftIO getCPUTime
        liftIO (createDirectoryIfMissing True outDirName)

        files <-
            toList <$> Rock.fetch Elara.Query.InputFiles

        loadedModules <- for ["source.elr"] $ \file -> Local.evalState primitiveRenameState $ do
            (Module (Located _ m)) <- runErrorOrReportEff @(WParseErrorBundle _ _) $ Rock.fetch $ Elara.Query.ParsedFile file
            runErrorOrReportEff @RenameError $ Rock.fetch $ Elara.Query.RenamedModule (m.name ^. unlocated)

        printPretty loadedModules
        -- let graph = createGraph $ toList loadedModules
        -- coreGraph <- processModules graph (dumpShunted, dumpTyped)
        -- when dumpCore $ do
        --     liftIO $ dumpGraph coreGraph (view (field' @"name" % to nameText)) ".core.elr"
        -- coreGraph <- uniqueGenToIO $ traverseGraph toANF' coreGraph
        -- anfCoreGraph <- uniqueGenToIO $ traverseGraph runLiftClosures coreGraph

        -- coreGraph <- traverseGraph (pure . unANF) anfCoreGraph

        -- -- override the core graph with the processed one
        -- when dumpCore $ do
        --     liftIO $ dumpGraph coreGraph (view (field' @"name" % to nameText)) ".core.elr"

        -- -- type check the core graph _after_ dumping for debugging purposes
        -- runErrorOrReport $ traverseGraph_ typeCheckCoreModule anfCoreGraph

        -- runInterpreter $ do
        --     flip traverseGraphRevTopologically_ coreGraph $ \mod -> do
        --         Interpreter.loadModule mod
        --     when True $ do
        --         Interpreter.run
        -- putTextLn (showPretty class')
        -- converted <- runErrorOrReport $ fromEither $ convert class'
        -- let bs = runPut (writeBinary converted)
        -- let fp = "build/" <> suitableFilePath class'.name
        -- liftIO $ createAndWriteFile fp bs
        -- putTextLn ("Compiled " <> showPretty (class'.name) <> " to " <> toText fp <> "!")
        -- classes <- runReader java8 (emitGraph coreGraph)
        -- for_ classes $ \(mn, classes') -> do
        --     putTextLn ("Compiling " <> showPretty mn <> "...")
        --     for_ classes' $ \class' -> do
        --         converted <- runErrorOrReport $ fromEither $ convert class'
        --         let bs = runPut (writeBinary converted)
        --         let fp = "build/" <> suitableFilePath class'.name
        --         liftIO $ createAndWriteFile fp bs
        --         putTextLn ("Compiled " <> showPretty mn <> " to " <> toText fp <> "!")
        --     putTextLn ("Successfully compiled " <> showPretty mn <> "!")

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
