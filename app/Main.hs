{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (
    main,
)
where

import Control.Exception as E
import Control.Lens (Each (each), folded, to, traverseOf_, view, (^.))
import Data.Binary.Put (runPut)
import Data.Binary.Write (WriteBinary (..))
import Data.ByteString.Char8 (putStrLn)
import Elara.AST.Module
import Elara.AST.Name (NameLike (..))
import Elara.AST.Region (unlocated)
import Elara.AST.Select (Core)
import Elara.AST.Select hiding (moduleName)
import Elara.AST.Typed qualified as Typed
import Elara.Data.Kind.Infer
import Elara.Data.Pretty
import Elara.Data.TopologicalGraph (TopologicalGraph, allEntries, createGraph, traverseGraph, traverseGraphRevTopologically, traverseGraph_)
import Elara.Data.Unique (resetGlobalUniqueSupply, uniqueGenToIO)
import Elara.Desugar (desugar, runDesugar)
import Elara.Emit
import Elara.Error
import Elara.Error (ReportableError)
import Elara.Error.Codes qualified as Codes (fileReadError)
import Elara.Lexer.Reader
import Elara.Lexer.Token (Lexeme)
import Elara.Lexer.Utils
import Elara.Parse
import Elara.Parse.Stream
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Rename (rename, runRenamer)
import Elara.Shunt
import Elara.ToCore (moduleToCore, runToCoreC, toCore)
import Elara.TypeInfer qualified as Infer
import Elara.TypeInfer.Infer (Status, initialStatus)
import Error.Diagnose (Diagnostic, Report (Err), defaultStyle, printDiagnostic)
import JVM.Data.Abstract.ClassFile as ClassFile hiding (name)
import JVM.Data.Abstract.ClassFile qualified as ClassFile
import JVM.Data.Abstract.Name (suitableFilePath)
import JVM.Data.Convert (convert)
import JVM.Data.JVMVersion
import Polysemy (Member, Members, Sem, raise, runM, subsume, subsume_)
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Maybe (MaybeE, justE, nothingE, runMaybe)
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer (runWriter)
import Prettyprinter.Render.Text
import Print
import Print (printPretty)
import System.CPUTime
import System.Directory (createDirectoryIfMissing)
import System.IO (openFile)
import Text.Printf

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
        printDiagnostic stdout True True 4 defaultStyle s
        pass

dumpGraph :: Pretty m => TopologicalGraph m -> (m -> Text) -> Text -> IO ()
dumpGraph graph nameFunc suffix = do
    let dump mod = do
            let contents = pretty mod
            let fileName = toString ("out/" <> nameFunc mod <> suffix)
            handle <- openFile fileName WriteMode
            hPutDoc handle contents
            hFlush handle

    traverseGraph_ dump graph

runElara :: Bool -> Bool -> Bool -> IO (Diagnostic (Doc AnsiStyle))
runElara dumpShunted dumpTyped dumpCore = runM $ execDiagnosticWriter $ runMaybe $ do
    start <- liftIO getCPUTime
    liftIO (createDirectoryIfMissing True "out")

    source <- loadModule "source.elr"
    prelude <- loadModule "prelude.elr"
    let graph = createGraph [source, prelude]
    shuntedGraph <- traverseGraph (renameModule graph >=> shuntModule) graph
    when dumpShunted $ do
        liftIO $ dumpGraph shuntedGraph (view (name . to nameText)) ".shunted.elr"

    typedGraph <- inferModules shuntedGraph

    when dumpTyped $ do
        liftIO $ dumpGraph typedGraph (view (name . to nameText)) ".typed.elr"

    coreGraph <- reportMaybe $ subsume $ uniqueGenToIO $ runToCoreC (traverseGraph moduleToCore typedGraph)

    when dumpCore $ do
        liftIO $ dumpGraph coreGraph (view (name . to nameText)) ".core.elr"

-- classes <- runReader java8 (emitGraph typedGraph)

-- for_ classes $ \(mn, class') -> do
--   putTextLn ("Compiling " <> showPretty mn <> "...")
--   let converted = convert class'
--   let bs = runPut (writeBinary converted)
--   liftIO $ writeFileLBS ("out/" <> suitableFilePath (ClassFile.name class')) bs
--   putTextLn ("Compiled " <> showPretty mn <> "!")

-- end <- liftIO getCPUTime
-- let t :: Double
--     t = fromIntegral (end - start) * 1e-9
-- putTextLn ("Successfully compiled " <> show (length classes) <> " classes in " <> fromString (printf "%.2f" t) <> "ms!")

cleanup :: IO ()
cleanup = resetGlobalUniqueSupply

type MainMembers = '[DiagnosticWriter (Doc AnsiStyle), MaybeE]

readFileString :: (Member (Embed IO) r, Members MainMembers r) => FilePath -> Sem r String
readFileString path = do
    contentsBS <- readFileBS path
    case decodeUtf8Strict contentsBS of
        Left err -> do
            writeReport (Err (Just Codes.fileReadError) ("Could not read " <> pretty path <> ": " <> show err) [] []) *> nothingE
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

parseModule :: (Members MainMembers r) => FilePath -> (String, [Lexeme]) -> Sem r (Module Frontend)
parseModule path (contents, lexemes) = do
    let tokenStream = TokenStream contents lexemes 0
    case parse path tokenStream of
        Left parseError -> do
            report parseError *> nothingE
        Right m -> do
            -- debugColored (stripLocation m)
            justE m

desugarModule :: (Members MainMembers r) => Module Frontend -> Sem r (Module Desugared)
desugarModule m = do
    case runDesugar (desugar m) of
        Left err -> report err *> nothingE
        Right desugared -> justE desugared

renameModule ::
    (Members MainMembers r, Member (Embed IO) r) =>
    TopologicalGraph (Module Desugared) ->
    Module Desugared ->
    Sem r (Module Renamed)
renameModule mp m = do
    y <- subsume $ runRenamer primitiveRenameState mp (rename m)
    case y of
        Left err -> report err *> nothingE
        Right renamed -> justE renamed

shuntModule :: (Members MainMembers r) => Module Renamed -> Sem r (Module Shunted)
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

inferModules :: (Members MainMembers r) => TopologicalGraph (Module Shunted) -> Sem r (TopologicalGraph (Module Typed))
inferModules modules = do
    runErrorOrReport (evalState @InferState initialInferState (evalState @Status initialStatus (traverseGraphRevTopologically Infer.inferModule modules)))

loadModule :: (Members MainMembers r, Member (Embed IO) r) => FilePath -> Sem r (Module Desugared)
loadModule fp = (lexFile >=> parseModule fp >=> desugarModule) fp

runErrorOrReport ::
    (Members MainMembers r, ReportableError e) =>
    Sem (Error e ': r) a ->
    Sem r a
runErrorOrReport e = do
    x <- subsume_ (runError e)
    case x of
        Left err -> report err *> nothingE
        Right a -> justE a

reportMaybe :: Member MaybeE r => Member (DiagnosticWriter (Doc AnsiStyle)) r => (ReportableError e) => Sem r (Either e a) -> Sem r a
reportMaybe x = do
    x' <- x
    case x' of
        Left err -> report err *> nothingE
        Right a -> justE a