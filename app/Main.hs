{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module
import Elara.AST.Select
import Elara.Annotate (annotateModule)
import Elara.Parse
import Polysemy (run)
import Polysemy.Error (runError)
import Polysemy.Reader
import Print (printColored)
import Text.Megaparsec (errorBundlePretty)
import Prelude hiding (runReader)

main :: IO ()
main = do
  s <- decodeUtf8Strict <$> readFileBS "source.elr"
  case s of
    Left err -> putStrLn ("Could not read file: " <> show err)
    Right file ->
      case parse "source.elr" file of
        Left err -> putStrLn $ errorBundlePretty err
        Right m -> do
          printColored (unlocateModule m)
          let modules = fromList [(m ^. name, m)]
          case run $ runError $ runReader modules (annotateModule m) of
            Left err -> printColored err
            Right m' -> printColored m'

unlocateModule :: Module Frontend -> Module UnlocatedFrontend
unlocateModule = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens %~ stripLocation