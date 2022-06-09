{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compile (compile)
import Data.Text
import Elara.Package qualified as Pkg
import Parse.Module
import Text.Megaparsec
import Print (printColored)

main :: IO ()
main = do
  content <- pack <$> readFile "source.elr"
  let res = runParser module' "source.elr" content
  case res of
    Left err -> putStrLn $ errorBundlePretty err
    Right moduleAST -> do
      let compiled = compile (Pkg.Name "test") moduleAST
      printColored compiled
