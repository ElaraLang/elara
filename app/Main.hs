{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main
  ( main,
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
import Elara.Lexer.Pipeline (runLexPipeline)
import Elara.Lexer.Reader
import Elara.Parse
import Elara.Pipeline (IsPipeline, finalisePipeline)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.ReadFile (readFileString, runReadFilePipeline)
import Elara.Rename (rename, runRenamePipeline)
import Elara.Shunt
import Elara.ToCore (moduleToCore, runToCorePipeline)
import Elara.TypeInfer
import Error.Diagnose (Diagnostic, TabSize (..), WithUnicode (..), defaultStyle, printDiagnostic')
import JVM.Data.Abstract.ClassFile qualified as ClassFile
import JVM.Data.Abstract.Name (suitableFilePath)
import JVM.Data.Convert (convert)
import JVM.Data.JVMVersion
import Polysemy (Sem)
import Polysemy.Reader
import Prettyprinter.Render.Text
import Print
import System.CPUTime
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.IO (openFile)
import Text.Printf

outDirName :: (IsString s) => s
outDirName = "build"

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
      s <- runElara dumpShunted dumpTyped dumpCore
      printDiagnostic' stdout WithUnicode (TabSize 4) defaultStyle s
      pass

dumpGraph :: (Pretty m) => TopologicalGraph m -> (m -> Text) -> Text -> IO ()
dumpGraph graph nameFunc suffix = do
  let dump m = do
        let contents = pretty m
        let fileName = toString (outDirName <> "/" <> nameFunc m <> suffix)
        fileHandle <- openFile fileName WriteMode
        hPutDoc fileHandle contents
        hFlush fileHandle

  traverseGraph_ dump graph

runElara :: Bool -> Bool -> Bool -> IO (Diagnostic (Doc AnsiStyle))
runElara dumpShunted dumpTyped dumpCore = fmap fst <$> finalisePipeline $ do
  start <- liftIO getCPUTime
  liftIO (createDirectoryIfMissing True outDirName)

  source <- loadModule "source.elr"
  prelude <- loadModule "prelude.elr"

  let graph = createGraph [source, prelude]
  coreGraph <- processModules graph (dumpShunted, dumpTyped)

  when dumpCore $ do
    liftIO $ dumpGraph coreGraph (view (field' @"name" . to nameText)) ".core.elr"

  classes <- runReader java8 (emitGraph coreGraph)
  for_ classes $ \(mn, class') -> do
    putTextLn ("Compiling " <> showPretty mn <> "...")
    let converted = convert class'
    let bs = runPut (writeBinary converted)
    let fp = "build/" <> suitableFilePath (ClassFile.name class')
    liftIO $ writeFileLBS fp bs
    putTextLn ("Compiled " <> showPretty mn <> " to " <> toText fp <> "!")

  end <- liftIO getCPUTime
  let t :: Double
      t = fromIntegral (end - start) * 1e-9
  putTextLn ("Successfully compiled " <> show (length classes) <> " classes in " <> fromString (printf "%.2f" t) <> "ms!")

-- typedGraph <- inferModules shuntedGraph

-- when dumpTyped $ do
--     liftIO
--         ( dumpGraph
--             typedGraph
--             (view (_Unwrapped . unlocated . field' @"name" . to nameText))
--             ".typed.elr"
--         )

-- coreGraph <- reportMaybe $ subsume $ uniqueGenToIO $ runToCoreC (traverseGraph moduleToCore typedGraph)

-- when dumpCore $ liftIO $ dumpGraph coreGraph (view (field' @"name" . to nameText)) ".core.elr"

-- classes <- runReader java8 (emitGraph coreGraph)

-- for_ classes $ \(mn, class') -> do
--     putTextLn ("Compiling " <> showPretty mn <> "...")
--     let converted = convert class'
--     let bs = runPut (writeBinary converted)
--     liftIO $ writeFileLBS ("build/" <> suitableFilePath (ClassFile.name class')) bs
--     putTextLn ("Compiled " <> showPretty mn <> "!")

-- end <- liftIO getCPUTime
-- let t :: Double
--     t = fromIntegral (end - start) * 1e-9
-- putTextLn ("Successfully compiled " <> show (length classes) <> " classes in " <> fromString (printf "%.2f" t) <> "ms!")

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply

-- renameModule ::
--     (Members MainMembers r, Member (Embed IO) r) =>
--     TopologicalGraph (Module 'Desugared) ->
--     Module 'Desugared ->
--     Sem r (Module 'Renamed)
-- renameModule mp m = do
--     y <- subsume $ runRenamer primitiveRenameState mp (rename m)
--     case y of
--         Left err -> report err *> nothingE
--         Right renamed -> justE renamed

-- shuntModule :: (Members MainMembers r) => Module 'Renamed -> Sem r (Module 'Shunted)
-- shuntModule m = do
--     x <-
--         runError $
--             runWriter $
--                 runReader (fromList []) (shunt m)
--     case x of
--         Left err -> report err *> nothingE
--         Right (warnings, shunted) -> do
--             traverse_ report warnings
--             justE shunted

loadModule :: (IsPipeline r) => FilePath -> Sem r (Module 'Desugared)
loadModule fp = runDesugarPipeline . runParsePipeline . runLexPipeline . runReadFilePipeline $ do
  source <- readFileString fp
  tokens <- readTokensWith fp source
  -- printColored (stripLocation @(Located Token) @Token <$> tokens)
  parsed <- parsePipeline moduleParser fp tokens
  runDesugarPipeline $ runDesugar $ desugar parsed

processModules :: (IsPipeline r) => TopologicalGraph (Module 'Desugared) -> (Bool, Bool) -> Sem r (TopologicalGraph CoreModule)
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
              >=> pure . mapGraph coreToCore
              $ graph
          )
  where
    dumpIf cond f p = if cond then (\x -> liftIO (dumpGraph x f p) $> x) else pure
