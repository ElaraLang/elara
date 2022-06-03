module Main where

import Data.Text
import Parse.Module
import Text.Megaparsec
import Text.Pretty.Simple
main = do
  content <- pack <$> readFile "source.elr"
  let res = runParser module' "source.elr" content
  printColored res

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg