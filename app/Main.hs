{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import Elara.Parse.Expression (expression)
import Elara.Parse.Module (module')
import Text.Megaparsec
import Text.Pretty.Simple

main :: IO ()
main = do
  content <- pack <$> readFile "source.elr"
  let res = runParser module' "source.elr" content
  case res of
    Left err -> putStrLn $ errorBundlePretty err
    Right moduleAST -> do
      printColored moduleAST

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg