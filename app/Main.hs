{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module
import Elara.AST.Select
import Elara.Annotate (annotateModule)
import Elara.Annotate.Shunt (fixOperators)
import Elara.Parse
import Polysemy (run)
import Polysemy.Error (runError)
import Polysemy.Reader
import Polysemy.Writer (runWriter)
import Print (printColored)
import Text.Megaparsec (errorBundlePretty)
import Prelude hiding (runReader)

main :: IO ()
main = do
  s <- loadModule "source.elr"
  p <- loadModule "prelude.elr"
  let sp = liftA2 (,) s p
  case sp of
    Left err -> printColored err
    Right (source, prelude) ->
      let modules = fromList [(source ^. name, source), (prelude ^. name, prelude)]
       in case run $ runError $ runReader modules (annotateModule source) of
            Left err -> printColored err
            Right m' -> do
              let y = run $ runError $ runWriter $ overExpressions (fixOperators (fromList [])) m'
              printColored y
          

loadModule :: FilePath -> IO (Either String (Module Frontend))
loadModule path = do
  s <- decodeUtf8Strict <$> readFileBS path
  case s of
    Left err -> pure (Left ("Could not read file: " <> show err))
    Right file ->
      case parse path file of
        Left err -> pure (Left (errorBundlePretty err))
        Right m -> pure (Right m)

unlocateModule :: Module Frontend -> Module UnlocatedFrontend
unlocateModule = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens %~ stripLocation

overExpressions :: Applicative f => (ASTExpr ast -> f (ASTExpr ast)) -> Module ast -> f (Module ast)
overExpressions = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens