{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module
import Elara.AST.Name (ModuleName (ModuleName))
import Elara.AST.Select
import Elara.Parse
import Elara.Parse.Declaration (declaration)
import Print (printColored)
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, runParser, sepBy)
import Text.Megaparsec.Char (char)
import Elara.Parse.Expression

main :: IO ()
main = do
  s <- readFileText "source.elr"
  case runParser exprParser "source.elr" s of
    Left err -> putStrLn $ errorBundlePretty err
    Right expr -> 
      printColored (stripLocation expr)

unlocateModule :: Module Frontend -> Module UnlocatedFrontend
unlocateModule = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens %~ stripLocation