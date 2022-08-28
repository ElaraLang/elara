{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens ((%~))
import Control.Lens.Getter (view)
import Elara.AST.Frontend (LocatedExpr, UnwrappedExpr, unlocateExpr)
import Elara.Data.Module
import Elara.Data.Module qualified as Mod
import Elara.Data.Prelude (prelude)
import Elara.Data.Uniqueness
import Elara.Desugar.Desugar (desugarModule)
import Elara.Parse
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple
  ( CheckColorTty (NoCheckColorTty),
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
      printColored (unlocateModule moduleAST)
      printColored desugared

printColored :: (Show a) => a -> IO ()
printColored = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg

unlocateModule :: Module LocatedExpr p a q 'Many -> Module UnwrappedExpr p a q 'Many
unlocateModule = Mod.moduleDeclarations . traverse . Mod.declarationBody . declarationBodyExpression %~ unlocateExpr