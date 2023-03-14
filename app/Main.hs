{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Module
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Select
import Elara.Annotate (annotateModule)
import Elara.Annotate.Shunt (fixOperators)
import Elara.Error
import Elara.Error.Effect (
  DiagnosticWriter,
  addFile,
  execDiagnosticWriter,
  writeDiagnostic,
  writeReport,
 )
import Elara.Lexer.Lexer
import Elara.Parse
import Elara.Parse.Stream
import Error.Diagnose (Diagnostic, Note (Note), Report (Err), defaultStyle, printDiagnostic)
import Error.Diagnose.Diagnostic (hasReports)
import Polysemy (Embed, Member, Sem, embed, run, runM)
import Polysemy.Error (runError)
import Polysemy.Reader
import Polysemy.Writer (runWriter)
import Print (printColored)
import Prelude hiding (State, evalState, execState, modify, runReader, runState)

main :: IO ()
main = do
  -- (runM $ lexFile "source.elr") <&> (fmap (fmap (view unlocated))) >>= printColored
  s <- runElara
  when (hasReports s) $ do
    printDiagnostic stdout True True 4 defaultStyle s
    exitFailure

runElara :: IO (Diagnostic Text)
runElara = runM $ execDiagnosticWriter $ do
  s <- loadModule "source.elr"
  p <- loadModule "prelude.elr"
  case liftA2 (,) s p of
    Nothing -> pass
    Just (source, prelude) ->
      let modules = fromList [(source ^. (name . unlocated), source), (prelude ^. (name . unlocated), prelude)]
       in case run $ runError $ runReader modules (annotateModule source) of
            Left annotateError -> report annotateError
            Right m' -> do
              fixOperatorsInModule m' >>= embed . printColored

fixOperatorsInModule :: (Member (DiagnosticWriter Text) r) => Module Annotated -> Sem r (Maybe (Module Annotated))
fixOperatorsInModule m = do
  let x =
        run $
          runError $
            runWriter $
              overExpressions
                ( fixOperators
                    ( fromList
                        []
                    )
                )
                m
  case x of
    Left shuntErr -> do
      report shuntErr $> Nothing
    Right (warnings, finalM) -> do
      traverse_ report (toList warnings)
      pure (Just finalM)

lexFile :: (Member (Embed IO) r) => FilePath -> Sem r (Either String [Lexeme])
lexFile path = do
  bs <- readFileLBS path
  pure (lex path bs)

loadModule :: (Member (Embed IO) r, Member (DiagnosticWriter Text) r) => FilePath -> Sem r (Maybe (Module Frontend))
loadModule path = do
  s <- readFileLBS path
  case decodeUtf8Strict s of
    Left unicodeError -> do
      let errReport = Err Nothing ("Could not read file: " <> fromString path) [] [Note (show unicodeError)]
       in writeReport errReport $> Nothing
    Right (contents :: Text) -> do
      let contentsAsString = toString contents
      addFile path contentsAsString -- add every loaded file to the diagnostic
      case lex path s of
        Left lexError -> do
          print lexError
          pure Nothing
        Right lexemes -> do
          let tokenStream = TokenStream contentsAsString lexemes
          case parse path tokenStream of
            Left parseError -> do
              report parseError $> Nothing
            Right m -> pure (Just m)

overExpressions ::
  ( UnwrapUnlocated (ASTLocate' ast2 (DeclarationBody' ast2))
      ~ Located (DeclarationBody' ast)
  , HasDeclarations s (t a)
  , Traversable t
  , Applicative f
  , HasBody a (DeclarationBody ast2)
  ) =>
  (ASTExpr ast -> f (ASTExpr ast)) ->
  s ->
  f s
overExpressions = declarations . traverse . body . _DeclarationBody . unlocated . expression