{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (
    main,
)
where

import Control.Exception as E
import Control.Lens (to, view)
import Data.Binary.Put (runPut)
import Data.Binary.Write (WriteBinary (..))
import Data.Generics.Product
import Data.Generics.Wrapped
import Elara.AST.Module
import Elara.AST.Name (NameLike (..))
import Elara.AST.Region (unlocated)
import Elara.AST.Select
import Elara.Core.Module (CoreModule)
import Elara.CoreToCore
import Elara.Data.Pretty
import Elara.Data.TopologicalGraph (TopologicalGraph, createGraph, mapGraph, traverseGraph, traverseGraphRevTopologically, traverseGraph_)
import Elara.Data.Unique (resetGlobalUniqueSupply)
import Elara.Desugar (desugar, runDesugar, runDesugarPipeline)
import Elara.Emit
import Elara.Error (ReportableError (report), runErrorOrReport, writeReport)
import Elara.Lexer.Pipeline (runLexPipeline)
import Elara.Lexer.Reader
import Elara.Parse
import Elara.Pipeline (IsPipeline, finalisePipeline)
import Elara.Prim
import Elara.Prim.Rename (primitiveRenameState)
import Elara.ReadFile (readFileString, runReadFilePipeline)
import Elara.Rename (rename, runRenamePipeline)
import Elara.Shunt
import Elara.ToCore (moduleToCore, runToCorePipeline)
import Elara.TypeInfer
import Error.Diagnose (Diagnostic, Report (..), TabSize (..), WithUnicode (..), defaultStyle, printDiagnostic')
import JVM.Data.Abstract.ClassFile qualified as ClassFile
import JVM.Data.Abstract.Name (suitableFilePath)
import JVM.Data.Convert (convert)
import JVM.Data.Convert.Monad
import JVM.Data.JVMVersion
import Polysemy (Sem)
import Polysemy.Error (fromEither)
import Polysemy.Reader
import Prettyprinter.Render.Text
import Print
import System.CPUTime
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.IO (openFile)
import System.Process
import Text.Printf
import System.FilePath

outDirName :: IsString s => s
outDirName = "build"

instance ReportableError CodeConverterError where
    report x =
        writeReport $
            Err
                (Nothing)
                (show x)
                []
                []

main :: IO ()
main = run `finally` cleanup
  where
    run :: IO ()
    run = do
        hSetBuffering stdout NoBuffering
        putTextLn "\n"
        args <- getArgs
        env <- getEnvironment
        let dumpShunted = "--dump-shunted" `elem` args || "ELARA_DUMP_SHUNTED" `elem` fmap fst env
        let dumpTyped = "--dump-typed" `elem` args || "ELARA_DUMP_TYPED" `elem` fmap fst env
        let dumpCore = "--dump-core" `elem` args || "ELARA_DUMP_CORE" `elem` fmap fst env
        let run = "--run" `elem` args || "ELARA_RUN" `elem` fmap fst env
        s <- runElara dumpShunted dumpTyped dumpCore run
        printDiagnostic' stdout WithUnicode (TabSize 4) defaultStyle s
        pass

dumpGraph :: Pretty m => TopologicalGraph m -> (m -> Text) -> Text -> IO ()
dumpGraph graph nameFunc suffix = do
    let dump m = do
            let contents = pretty m
            let fileName = toString (outDirName <> "/" <> nameFunc m <> suffix)
            fileHandle <- openFile fileName WriteMode
            hPutDoc fileHandle contents
            hFlush fileHandle

    traverseGraph_ dump graph

runElara :: Bool -> Bool -> Bool -> Bool -> IO (Diagnostic (Doc AnsiStyle))
runElara dumpShunted dumpTyped dumpCore run = fmap fst <$> finalisePipeline $ do
    start <- liftIO getCPUTime
    liftIO (createDirectoryIfMissing True outDirName)

    prim <- loadModule "prim.elr"
    source <- loadModule "source.elr"
    prelude <- loadModule "prelude.elr"

    let graph = createGraph [prim, source, prelude]
    coreGraph <- processModules graph (dumpShunted, dumpTyped)

    when dumpCore $ do
        liftIO $ dumpGraph coreGraph (view (field' @"name" . to nameText)) ".core.elr"

    classes <- runReader java8 (emitGraph coreGraph)
    for_ classes $ \(mn, class') -> do
        putTextLn ("Compiling " <> showPretty mn <> "...")
        converted <- runErrorOrReport $ fromEither $ convert class'
        let bs = runPut (writeBinary converted)
        let fp = "build/" <> suitableFilePath (class'.name)
        liftIO $ createAndWriteFile fp bs
        putTextLn ("Compiled " <> showPretty mn <> " to " <> toText fp <> "!")

    end <- liftIO getCPUTime
    let t :: Double
        t = fromIntegral (end - start) * 1e-9
    putTextLn ("Successfully compiled " <> show (length classes) <> " classes in " <> fromString (printf "%.2f" t) <> "ms!")

    when run $ liftIO $ do
        -- run 'java -cp ../jvm-stdlib:. Main' in pwd = './build'
        x <- readCreateProcess ((shell "java -cp ../jvm-stdlib:. Main"){cwd = Just "./build"}) ""
        putStrLn x

createAndWriteFile :: FilePath -> LByteString -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path

  writeFileLBS path content

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply

loadModule :: IsPipeline r => FilePath -> Sem r (Module 'Desugared)
loadModule fp = runDesugarPipeline . runParsePipeline . runLexPipeline . runReadFilePipeline $ do
    source <- readFileString fp
    tokens <- readTokensWith fp source

    parsed <- parsePipeline moduleParser fp tokens
    runDesugarPipeline $ runDesugar $ desugar parsed

processModules :: IsPipeline r => TopologicalGraph (Module 'Desugared) -> (Bool, Bool) -> Sem r (TopologicalGraph CoreModule)
processModules graph (dumpShunted, dumpTyped) =
    runToCorePipeline $
        runInferPipeline $
            runShuntPipeline mempty $
                runRenamePipeline
                    graph
                    primitiveRenameState
                    ( traverseGraph rename
                        >=> traverseGraph shunt
                        >=> dumpIf dumpShunted (view (_Unwrapped . unlocated . field' @"name" . to nameText)) ".shunted.elr"
                        >=> traverseGraphRevTopologically inferModule
                        >=> dumpIf dumpTyped (view (_Unwrapped . unlocated . field' @"name" . to nameText)) ".typed.elr"
                        >=> traverseGraph moduleToCore
                        >=> pure
                        . mapGraph coreToCore
                        $ graph
                    )
  where
    dumpIf cond f p = if cond then (\x -> liftIO (dumpGraph x f p) $> x) else pure
