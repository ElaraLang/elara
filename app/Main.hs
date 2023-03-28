{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Module
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Renamed
import Elara.AST.Select
import Elara.Error
import Elara.Error.Codes qualified as Codes (fileReadError)
import Elara.Error.Effect (
  DiagnosticWriter,
  addFile,
  execDiagnosticWriter,
  writeReport,
 )
import Elara.Lexer.Lexer
import Elara.Lexer.Reader
import Elara.Lexer.Token (Lexeme)
import Elara.Lexer.Utils
import Elara.Parse
import Elara.Parse.Stream
import Error.Diagnose (Diagnostic, Note (Note), Report (Err), defaultStyle, printDiagnostic)
import Error.Diagnose.Diagnostic (hasReports)
import Polysemy (Embed, Member, Sem, embed, run, runM)
import Polysemy.Error (runError)
import Polysemy.Maybe (MaybeE, justE, nothingE, runMaybe)
import Polysemy.Reader
import Polysemy.Writer (runWriter)
import Print (printColored)
import Prelude hiding (State, evalState, execState, modify, runReader, runState)

main :: IO ()
main = do
  s <- runElara
  case s of
    Nothing -> pass
    Just s -> when (hasReports s) $ do
      printDiagnostic stdout True True 4 defaultStyle s
      exitFailure

runElara :: IO (Maybe (Diagnostic Text))
runElara = runM $ runMaybe $ execDiagnosticWriter $ do
  s <- loadModule "source.elr"
  p <- loadModule "prelude.elr"
  case liftA2 (,) s p of
    Nothing -> pass
    Just (source, prelude) ->
      embed (printColored source)

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
    Right lexemes -> justE (contents, lexemes)

loadModule :: (Member (Embed IO) r, Member (DiagnosticWriter Text) r, Member MaybeE r) => FilePath -> Sem r (Maybe (Module Frontend))
loadModule path = do
  (contents, lexemes) <- lexFile path
  let tokenStream = TokenStream contents lexemes
  case parse path tokenStream of
    Left parseError -> do
      report parseError $> Nothing
    Right m -> pure (Just m)

-- overExpressions = declarations . traverse . _Declaration . unlocated . declaration'Body . _DeclarationBody . unlocated . expression