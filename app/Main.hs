{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module
import Elara.AST.Select
import Elara.Parse
import Print (printColored)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  s <- decodeUtf8Strict <$> readFileBS "source.elr"
  case s of
    Left err -> putStrLn ("Could not read file: " <> show err)
    Right file ->
      case parse "source.elr" file of
        Left err -> putStrLn $ errorBundlePretty err
        Right expr ->
          printColored (unlocateModule expr)

unlocateModule :: Module Frontend -> Module UnlocatedFrontend
unlocateModule = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens %~ stripLocation