{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (pack)
import Elara.Data.Module
import Elara.Data.Prelude (prelude)
import Elara.Desugar.Desugar (desugarModule)
import Elara.Parse
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple
  ( CheckColorTty (NoCheckColorTty),
    defaultOutputOptionsDarkBg,
    pPrintOpt,
  )
import Utils qualified
import Control.Lens.Getter (view)

main :: IO ()
main = do
  content <- decodeUtf8 <$> readFileBS  "source.elr"
  let res = parse "source.elr" content
  case res of
    Left err -> putStrLn $ errorBundlePretty err
    Right moduleAST -> do
      let modules = Utils.associateWithKey (view name) [prelude, moduleAST]
      let desugared = desugarModule modules moduleAST
      printColored moduleAST
      printColored desugared

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg