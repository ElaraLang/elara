{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens.Getter (view)
import Data.Map (elems)
import Elara.Data.Module
import Elara.Data.Prelude (prelude)
import Elara.Desugar.Desugar (desugarModule)
import Elara.Parse
import Elara.TypeInfer.Declaration (inferDeclaration)
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Infer
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
  let res = parse "source.elr" content
  case res of
    Left err -> putStrLn $ errorBundlePretty err
    Right moduleAST -> do
      let modules = Utils.associateWithKey (view name) [prelude, moduleAST]
      let desugared = desugarModule modules moduleAST
      case desugared of
        Left err -> print err
        Right module' -> do
          let decs = elems (view declarations module')
          let res = runInfer emptyEnvironment $ forM decs inferDeclaration
          printColored res

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg