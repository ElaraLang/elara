{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Module
import Elara.AST.Select
import Elara.Annotate (annotateModule)

import Elara.Annotate.Shunt (fixOperators)

import Elara.AST.Region (Located, _Unlocate)
import Elara.Error
import Elara.Error.Effect (DiagnosticWriter, addReport, execDiagnosticWriter, runDiagnosticWriter)
import Elara.Parse

import Elara.Error.Effect (addDiagnostic, addFile)
import Error.Diagnose (Diagnostic, Note (Note), Report (Err), defaultStyle, printDiagnostic)
import Error.Diagnose.Diagnostic (hasReports)
import Polysemy (Embed, Member, Sem, embed, run, runM)
import Polysemy.Error (runError)
import Polysemy.Reader
import Polysemy.State (State, execState, modify)
import Polysemy.Writer (runWriter)
import Print (printColored)
import Prelude hiding (State, evalState, execState, modify, runReader, runState)

main :: IO ()
main = do
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
      let modules = fromList [(source ^. (name . _Unlocate), source), (prelude ^. (name . _Unlocate), prelude)]
       in case run $ runError $ runReader modules (annotateModule source) of
            Left annotateError -> runFileContentsIO $ reportDiagnostic annotateError >>= addDiagnostic
            Right m' -> do
              runFileContentsIO $ fixOperatorsInModule m' >>= embed . printColored

fixOperatorsInModule :: (Member (DiagnosticWriter Text) r, Member FileContents r) => Module Annotated -> Sem r (Maybe (Module Annotated))
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
      diag <- reportDiagnostic shuntErr
      addDiagnostic diag
      pure Nothing
    Right (warnings, finalM) -> do
      warnings' <- traverse report (toList warnings)
      for_ warnings' addReport
      pure (Just finalM)

loadModule :: (Member (Embed IO) r, Member (DiagnosticWriter Text) r) => FilePath -> Sem r (Maybe (Module Frontend))
loadModule path = do
  s <- decodeUtf8Strict <$> readFileBS path
  case s of
    Left unicodeError -> do
      let errReport = Err Nothing ("Could not read file: " <> fromString path) [] [Note (show unicodeError)]
       in addReport errReport $> Nothing
    Right contents -> do
      addFile path (toString contents) -- add every loaded file to the diagnostic
      case parse path contents of
        Left parseError -> do
          diag <- runFileContentsIO $ reportDiagnostic parseError
          addDiagnostic diag $> Nothing
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
overExpressions = declarations . traverse . body . _DeclarationBody . _Unlocate . expression