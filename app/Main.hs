{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (
    main,
)
where

import Control.Exception as E
import Control.Lens (to, view)
import Data.Aeson (ToJSON, encode)
import Data.Binary.Put (runPut)
import Data.Binary.Write (WriteBinary (..))
import Data.Generics.Product
import Data.Generics.Wrapped
import Elara.AST.Module
import Elara.AST.Name (NameLike (..))
import Elara.AST.Region (unlocated)
import Elara.AST.Select
import Elara.Data.Kind.Infer
import Elara.Data.Pretty
import Elara.Data.TopologicalGraph (TopologicalGraph, createGraph, traverseGraph, traverseGraphRevTopologically, traverseGraph_)
import Elara.Data.Unique (resetGlobalUniqueSupply, uniqueGenToIO)
import Elara.Desugar (desugar, runDesugar)
import Elara.Emit
import Elara.Error
import Elara.Error.Codes qualified as Codes (fileReadError)
import Elara.Lexer.Reader
import Elara.Lexer.Token (Lexeme)
import Elara.Lexer.Utils
import Elara.Parse
import Elara.Parse.Stream
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Rename (rename, runRenamer)
import Elara.Shunt
import Elara.ToCore (moduleToCore, runToCoreC)
import Elara.TypeInfer qualified as Infer
import Elara.TypeInfer.Infer (Status, initialStatus)
import Error.Diagnose (Diagnostic, Report (Err), TabSize (..), WithUnicode (..), defaultStyle, printDiagnostic)
import JVM.Data.Abstract.ClassFile qualified as ClassFile
import JVM.Data.Abstract.Name (suitableFilePath)
import JVM.Data.Convert (convert)
import JVM.Data.JVMVersion
import Polysemy (Member, Members, Sem, runM, subsume)
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Maybe (MaybeE, justE, nothingE, runMaybe)
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer (runWriter)
import Prettyprinter.Render.Text
import Print
import System.CPUTime
import System.Directory (createDirectoryIfMissing)
import System.IO (openFile)
import Text.Printf

outDirName :: IsString s => s
outDirName = "build"

main :: IO ()
main = run `finally` cleanup
  where
    run :: IO ()
    run = do
        hSetBuffering stdout NoBuffering
        putTextLn "\n"
        args <- getArgs
        let dumpShunted = "--dump-shunted" `elem` args
        let dumpTyped = "--dump-typed" `elem` args
        let dumpCore = "--dump-core" `elem` args
        s <- runElara dumpShunted dumpTyped dumpCore
        printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle s
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

runElara :: Bool -> Bool -> Bool -> IO (Diagnostic (Doc AnsiStyle))
runElara dumpShunted dumpTyped dumpCore = runM $ execDiagnosticWriter $ runMaybe $ do
    start <- liftIO getCPUTime
    liftIO (createDirectoryIfMissing True outDirName)

    source <- loadModule "source.elr"
    prelude <- loadModule "prelude.elr"
    let graph = createGraph [source, prelude]
    shuntedGraph <- traverseGraph (renameModule graph >=> shuntModule) graph

    when dumpShunted $ do
        liftIO
            ( dumpGraph
                shuntedGraph
                (view (_Unwrapped . unlocated . field' @"name" . to nameText))
                ".shunted.elr"
            )

    typedGraph <- inferModules shuntedGraph

    when dumpTyped $ do
        liftIO
            ( dumpGraph
                typedGraph
                (view (_Unwrapped . unlocated . field' @"name" . to nameText))
                ".typed.elr"
            )

    coreGraph <- reportMaybe $ subsume $ uniqueGenToIO $ runToCoreC (traverseGraph moduleToCore typedGraph)

    when dumpCore $ liftIO $ dumpGraph coreGraph (view (field' @"name" . to nameText)) ".core.elr"

    classes <- runReader java8 (emitGraph coreGraph)

    for_ classes $ \(mn, class') -> do
        putTextLn ("Compiling " <> showPretty mn <> "...")
        let converted = convert class'
        let bs = runPut (writeBinary converted)
        liftIO $ writeFileLBS ("build/" <> suitableFilePath (ClassFile.name class')) bs
        putTextLn ("Compiled " <> showPretty mn <> "!")

    end <- liftIO getCPUTime
    let t :: Double
        t = fromIntegral (end - start) * 1e-9
    putTextLn ("Successfully compiled " <> show (length classes) <> " classes in " <> fromString (printf "%.2f" t) <> "ms!")

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply

type MainMembers = '[DiagnosticWriter (Doc AnsiStyle), MaybeE]

readFileString :: (Member (Embed IO) r, Members MainMembers r) => FilePath -> Sem r String
readFileString path = do
    contentsBS <- readFileBS path
    case decodeUtf8Strict contentsBS of
        Left err -> writeReport (Err (Just Codes.fileReadError) ("Could not read " <> pretty path <> ": " <> show err) [] []) *> nothingE
        Right contents -> do
            addFile path contents
            justE contents

lexFile :: (Member (Embed IO) r, Members MainMembers r) => FilePath -> Sem r (String, [Lexeme])
lexFile path = do
    contents <- readFileString path
    case evalLexMonad path contents readTokens of
        Left err -> report err *> nothingE
        Right lexemes -> do
            -- debugColored (stripLocation <$> lexemes)
            justE (contents, lexemes)

parseModule :: (Members MainMembers r) => FilePath -> (String, [Lexeme]) -> Sem r (Module 'Frontend)
parseModule path (contents, lexemes) = do
    let tokenStream = TokenStream contents lexemes 0
    case parse path tokenStream of
        Left parseError -> report parseError *> nothingE
        Right m -> do
            -- debugColored (stripLocation m)
            justE m

desugarModule :: (Members MainMembers r) => Module 'Frontend -> Sem r (Module 'Desugared)
desugarModule m = case runDesugar (desugar m) of
    Left err -> report err *> nothingE
    Right desugared -> justE desugared

renameModule ::
    (Members MainMembers r, Member (Embed IO) r) =>
    TopologicalGraph (Module 'Desugared) ->
    Module 'Desugared ->
    Sem r (Module 'Renamed)
renameModule mp m = do
    y <- subsume $ runRenamer primitiveRenameState mp (rename m)
    case y of
        Left err -> report err *> nothingE
        Right renamed -> justE renamed

shuntModule :: (Members MainMembers r) => Module 'Renamed -> Sem r (Module 'Shunted)
shuntModule m = do
    x <-
        runError $
            runWriter $
                runReader (fromList []) (shunt m)
    case x of
        Left err -> report err *> nothingE
        Right (warnings, shunted) -> do
            traverse_ report warnings
            justE shunted

inferModules :: (Members MainMembers r) => TopologicalGraph (Module 'Shunted) -> Sem r (TopologicalGraph (Module 'Typed))
inferModules modules = runErrorOrReport (evalState @InferState initialInferState (evalState @Status initialStatus (traverseGraphRevTopologically Infer.inferModule modules)))

loadModule :: (Members MainMembers r, Member (Embed IO) r) => FilePath -> Sem r (Module 'Desugared)
loadModule fp = (lexFile >=> parseModule fp >=> desugarModule) fp
