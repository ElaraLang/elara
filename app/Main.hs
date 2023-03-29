{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (
  main,
) where

import Control.Lens (view, (^.))
import Elara.AST.Module
import Elara.AST.Region (unlocated)
import Elara.AST.Select
import Elara.Data.Unique (uniqueGenToIO)
import Elara.Desugar (desugar, runDesugar)
import Elara.Error
import Elara.Error.Codes qualified as Codes (fileReadError)
import Elara.Lexer.Reader
import Elara.Lexer.Token (Lexeme)
import Elara.Lexer.Utils
import Elara.Parse
import Elara.Parse.Stream
import Elara.Rename (ModulePath, rename, runRenamer)
import Error.Diagnose (Diagnostic, Report (Err), defaultStyle, printDiagnostic)
import Polysemy (Embed, Member, Sem, embed, raise, raiseUnder, raise_, run, runM, subsume, subsume_)
import Polysemy.Maybe (MaybeE, justE, nothingE, runMaybe)
import Print (printColored)
import Prelude hiding (State, evalState, execState, modify, runReader, runState)

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
  pass

-- embed (printColored source')

-- runElara :: IO (Diagnostic Text)
-- runElara = runM $ execDiagnosticWriter $ do
--   s <- loadModule "source.elr"
--   p <- loadModule "prelude.elr"
--   case liftA2 (,) s p of
--     Nothing -> pass
--     Just (source, prelude) ->
--       embed (printColored source)

--  case run $ runError $ runReader modules (annotateModule source) of
--     Left annotateError -> report annotateError
--     Right m' -> do
--       fixOperatorsInModule m' >>= embed . printColored
--  case run $ runError $ runReader modules (annotateModule source) of
--     Left annotateError -> report annotateError
--     Right m' -> do
--       fixOperatorsInModule m' >>= embed . printColored

-- fixOperatorsInModule :: (Member (DiagnosticWriter Text) r) => Module Annotated -> Sem r (Maybe (Module Annotated))
-- fixOperatorsInModule m = do
--   let x =
--         run $
--           runError $
--             runWriter $
--               overExpressions
--                 ( fixOperators
--                     ( fromList
--                         []
--                     )
--                 )
--                 m
--   case x of
--     Left shuntErr -> do
--       report shuntErr $> Nothing
--     Right (warnings, finalM) -> do
--       traverse_ report (toList warnings)
--       pure (Just finalM)

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

loadModule :: (Member (DiagnosticWriter Text) r, Member (Embed IO) r, Member MaybeE r) => FilePath -> Sem r (Module Desugared)
loadModule fp = (lexFile >=> parseModule fp >=> desugarModule) fp

-- overExpressions = declarations . traverse . _Declaration . unlocated . declaration'Body . _DeclarationBody . unlocated . expression