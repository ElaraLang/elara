{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens.Getter (view)
import Elara.Data.Module
import Elara.Data.Prelude (prelude)
import Elara.Desugar.Desugar (desugarModule)
import Elara.Parse
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Infer (execInfer)
import Elara.TypeInfer.Module (inferModule)
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (
  CheckColorTty (NoCheckColorTty),
  defaultOutputOptionsDarkBg,
  pPrintOpt,
 )
import Utils qualified

main :: IO ()
main = do
  content <- decodeUtf8 <$> readFileBS "source.elr"
  let moduleAST = either (error . toText . errorBundlePretty) id (parse "source.elr" content)
  let modules = Utils.associateWithKey (view name) [prelude, moduleAST]
  let desugared = desugarModule modules moduleAST
  case desugared of
    Left err -> print err
    Right module' -> do
      let res = either (error . show) id (execInfer emptyEnvironment $ inferModule module')
      printColored res

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg