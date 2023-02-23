{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Module
import Elara.AST.Select
import Elara.Annotate (annotateModule)

-- import Elara.Annotate.Shunt (fixOperators)

import Elara.AST.Region (_Unlocate)
import Elara.Error
import Elara.Parse
import Error.Diagnose
import Polysemy (Embed, Member, Sem, embed, run, runM)
import Polysemy.Error (runError)
import Polysemy.Reader
import Polysemy.State (State, execState, modify)
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
              embed (printColored m')

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
          modify (<> diag)
          pure Nothing
        Right m -> pure (Just m)

-- unlocateModule :: Module Frontend -> Module UnlocatedFrontend
-- unlocateModule = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens %~ stripLocation

-- overExpressions :: Applicative f => (ASTExpr ast -> f (ASTExpr ast)) -> Module ast -> f (Module ast)
-- overExpressions = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens