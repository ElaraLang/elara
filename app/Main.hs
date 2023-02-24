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
import Elara.Parse
import Error.Diagnose
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
runElara = runM $ execState def $ do
  s <- loadModule "source.elr"
  p <- loadModule "prelude.elr"
  case liftA2 (,) s p of
    Nothing -> pass
    Just (source, prelude) ->
      let modules = fromList [(source ^. (name . _Unlocate), source), (prelude ^. (name . _Unlocate), prelude)]
       in case run $ runError $ runReader modules (annotateModule source) of
            Left annotateError -> runFileContentsIO $ reportDiagnostic annotateError >>= modify . flip (<>)
            Right m' -> do
              runFileContentsIO $ fixOperatorsInModule m' >>= embed . printColored

fixOperatorsInModule :: (Member (State (Diagnostic Text)) r, Member FileContents r) => Module Annotated -> Sem r (Maybe (Module Annotated))
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
      modify (concatDiagnostics diag)
      pure Nothing
    Right (warnings, finalM) -> do
      warnings' <- traverse report (toList warnings)
      for_ warnings' (modify . flip addReport)
      pure (Just finalM)

loadModule :: (Member (Embed IO) r, Member (State (Diagnostic Text)) r) => FilePath -> Sem r (Maybe (Module Frontend))
loadModule path = do
  s <- decodeUtf8Strict <$> readFileBS path
  case s of
    Left unicodeError -> do
      let errReport = Err Nothing ("Could not read file: " <> fromString path) [] [Note (show unicodeError)]
       in modify (`addReport` errReport) $> Nothing
    Right contents -> do
      modify (\x -> addFile x path (toString contents)) -- add every loaded file to the diagnostic
      case parse path contents of
        Left parseError -> do
          diag <- runFileContentsIO $ reportDiagnostic parseError
          modify (concatDiagnostics diag)
          pure Nothing
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