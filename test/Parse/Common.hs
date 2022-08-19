module Parse.Common where

import Control.Lens ((%~), (^.))
import Control.Lens.Plated (transformOn)
import Data.Map qualified as M
import Data.Text
import Elara.AST.Frontend as AST
import Elara.Data.Located
import Elara.Data.Module as Mod
import Elara.Data.Name
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.Type (TypeOrId' (Id))
import Elara.Data.TypeAnnotation (TypeAnnotation)
import Elara.Parse (parse)
import Elara.Parse.Primitives (Parser)
import NeatInterpolation
import System.Exit
import Test.HUnit.Lang
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, runParser)

testParse :: Text -> IO (Module UnwrappedExpr Pattern TypeAnnotation MaybeQualified)
testParse source = case parse "" source of
  Left err -> assertFailure (errorBundlePretty err)
  Right ast -> return (Mod.moduleDeclarations . traverse . Mod.declarationBody . declarationBodyExpression %~ unlocateExpr $ ast)

testParse' :: Text -> Parser a -> IO a
testParse' source parser = case runParser (parser <* eof) "" source of
  Left err -> assertFailure (errorBundlePretty err)
  Right ast -> return ast

(<:) :: Text -> Declaration UnwrappedExpr Pattern TypeAnnotation MaybeQualified -> IO ()
(<:) source decl = do
  ast <- testParse source
  M.elems (ast ^. declarations) `shouldContain` [decl]

(<=>) :: Text -> Module UnwrappedExpr Pattern TypeAnnotation MaybeQualified -> IO ()
(<=>) source expected = do
  ast <- testParse source
  ast `shouldBe` expected