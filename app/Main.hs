{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Module
import Elara.AST.Select
import Elara.Desugar (desugar, runDesugar)
import Elara.Error
import Elara.Error.Codes qualified as Codes (fileReadError)
import Elara.Lexer.Reader
import Elara.Lexer.Token (Lexeme)
import Elara.Lexer.Utils
import Elara.Parse
import Elara.Parse.Stream
import Elara.Rename (ModulePath, rename, runRenamer)
import Elara.Shunt
import Elara.TypeInfer qualified as Infer
import Elara.TypeInfer.Infer (initialStatus)
import Error.Diagnose (Diagnostic, Report (Err), defaultStyle, printDiagnostic)
import Polysemy (Embed, Member, Sem, runM, subsume_)
import Polysemy.Error
import Polysemy.Maybe (MaybeE, justE, nothingE, runMaybe)
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer (runWriter)
import Prettyprinter.Render.Text
import Elara.Data.Pretty
import Polysemy.Embed

main :: IO ()
main = do
  s <- runElara
  printDiagnostic stdout True True 4 defaultStyle s

runElara :: IO (Diagnostic Text)
runElara = runM $ execDiagnosticWriter $ runMaybe $ do
  source <- loadModule "source.elr"
  prelude <- loadModule "prelude.elr"
  let path = fromList [(source ^. unlocatedModuleName, source), (prelude ^. unlocatedModuleName, prelude)]
  source' <- renameModule path source
  source'' <- shuntModule source'
  source''' <- inferModule source''
  embed (putDoc (pretty source'''))
  putStrLn ""
  pass

readFileString :: (Member (Embed IO) r, Member (DiagnosticWriter Text) r, Member MaybeE r) => FilePath -> Sem r String
readFileString path = do
  contentsBS <- readFileBS path
  case decodeUtf8Strict contentsBS of
    Left err -> do
      writeReport (Err (Just Codes.fileReadError) ("Could not read " <> toText path <> ": " <> show err) [] []) *> nothingE
    Right contents -> do
      addFile path contents
      justE contents

lexFile :: (Member (Embed IO) r, Member (DiagnosticWriter Text) r, Member MaybeE r) => FilePath -> Sem r (String, [Lexeme])
lexFile path = do
  contents <- readFileString path
  case evalLexMonad path contents readTokens of
    Left err -> report err *> nothingE
    Right lexemes -> do
      -- embed (printColored (fmap (view unlocated) lexemes)) -- DEBUG
      justE (contents, lexemes)

parseModule :: (Member (DiagnosticWriter Text) r, Member MaybeE r) => FilePath -> (String, [Lexeme]) -> Sem r (Module Frontend)
parseModule path (contents, lexemes) = do
  let tokenStream = TokenStream contents lexemes
  case parse path tokenStream of
    Left parseError -> do
      report parseError *> nothingE
    Right m -> justE m

desugarModule :: (Member (DiagnosticWriter Text) r, Member MaybeE r) => Module Frontend -> Sem r (Module Desugared)
desugarModule m = do
  case runDesugar (desugar m) of
    Left err -> report err *> nothingE
    Right desugared -> justE desugared

renameModule ::
  (Member (DiagnosticWriter Text) r, Member MaybeE r, Member (Embed IO) r) =>
  ModulePath ->
  Module Desugared ->
  Sem r (Module Renamed)
renameModule mp m = do
  y <- subsume_ $ runRenamer mp (rename m)
  case y of
    Left err -> report err *> nothingE
    Right renamed -> justE renamed

shuntModule :: (Member (DiagnosticWriter Text) r, Member MaybeE r) => Module Renamed -> Sem r (Module Shunted)
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

inferModule :: (Member (DiagnosticWriter Text) r, Member MaybeE r, Member (Embed IO) r) => Module Shunted -> Sem r (Module _)
inferModule m = do
  runErrorOrReport (evalState initialStatus (Infer.inferModule m))

loadModule :: (Member (DiagnosticWriter Text) r, Member (Embed IO) r, Member MaybeE r) => FilePath -> Sem r (Module Desugared)
loadModule fp = (lexFile >=> parseModule fp >=> desugarModule) fp

runErrorOrReport ::
  forall e r a.
  (Member (DiagnosticWriter Text) r, Member MaybeE r, ReportableError e) =>
  Sem (Error e ': r) a ->
  Sem r a
runErrorOrReport e = do
  x <- subsume_ (runError e)
  case x of
    Left err -> report err *> nothingE
    Right a -> justE a
