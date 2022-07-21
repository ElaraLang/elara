{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (pack)
import Elara.Data.Module (_name)
import Elara.Data.Prelude (prelude)
import Elara.Desugar.Desugar (desugarModule)
import Elara.Parse.Module (module')
import Text.Megaparsec
import Text.Pretty.Simple
import Utils qualified

main :: IO ()
main = do
  content <- pack <$> readFile "source.elr"
  let res = runParser module' "source.elr" content
  case res of
    Left err -> putStrLn $ errorBundlePretty err
    Right moduleAST -> do
      let modules = Utils.associateWithKey _name [prelude, moduleAST]
      let desugared = desugarModule modules moduleAST
      printColored moduleAST
      printColored desugared

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg