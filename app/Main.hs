{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens (
  (%~),
  (^?),
 )
import Control.Lens.Getter (view)
import Data.Map (elems)
import Elara.AST.Frontend (
  LocatedExpr,
  UnwrappedExpr,
  unlocateExpr,
 )
import Elara.Data.Located
import Elara.Data.Module
import Elara.Data.Module qualified as Mod
import Elara.Data.Prelude (prelude)
import Elara.Data.Uniqueness
import Elara.Desugar.Desugar (desugarModule)
import Elara.Parse
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Expression
import Elara.TypeInfer.Infer
import Print (debugColored)
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
          let expressions =
                (\(a, _, _) -> unlocate a)
                  <$> mapMaybe
                    ((^? _Value) . view body)
                    (elems $ view declarations module')
          let res = runInfer emptyEnvironment $ forM expressions inferExpr
          printColored res

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg

unlocateModule ::
  Module LocatedExpr p a q 'Many -> Module UnwrappedExpr p a q 'Many
unlocateModule =
  Mod.moduleDeclarations
    . traverse
    . Mod.declarationBody
    . declarationBodyExpression
    %~ unlocateExpr
