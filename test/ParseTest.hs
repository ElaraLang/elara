module ParseTest where

import Control.Lens ((^.), (%~))
import Control.Lens.Plated (transformOn)
import Data.Map qualified as M
import Data.Text
import Elara.AST.Frontend as AST
import Elara.Data.Located
import Elara.Data.Module
import Elara.Data.Module as Mod
import Elara.Data.Name
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.Type (TypeOrId' (Id))
import Elara.Data.TypeAnnotation (TypeAnnotation)
import Elara.Parse (parse)
import NeatInterpolation
import System.Exit
import Test.HUnit.Lang
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Text.Megaparsec (errorBundlePretty)


spec :: Spec
spec = describe "Test Parser" $ do
  it "Parses a simple let declaration" $ do
    "let x = 1"
      <: Declaration
        { _declarationModule_ = ModuleName ["Main"],
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression = Identity (Int 1),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }

  it "Parses an indented let declaration" $
    do
      [text|
      let x = 
        1
      |]
      <: Declaration
        { _declarationModule_ = ModuleName ["Main"],
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression = Identity (Int 1),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }
  it "Parses an indented let declaration with multiple lines" $
    do
      [text|
      let x = 
        1
        2
      |]
      <: Declaration
        { _declarationModule_ = ModuleName ["Main"],
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression =
                  Identity
                    ( Block
                        [ Identity (Int 1),
                          Identity (Int 2)
                        ]
                    ),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }

  it "Parses a normal let with an indented lambda" $
    do
      [text|
      let x = \y ->
               1
      |]
      <: Declaration
        { _declarationModule_ = ModuleName ["Main"],
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression =
                  Identity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body = Identity (Int 1)
                        }
                    ),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }
  it "Parses a normal let with a non-indented lambda" $
    do
      [text|
      let x = 
        \y -> 1
      |]
      <: Declaration
        { _declarationModule_ = ModuleName ["Main"],
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression =
                  Identity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body =
                            Identity
                              ( Int 1
                              )
                        }
                    ),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }
  it "Parses an indented let with an indented lambda" $
    do
      [text|
      let x = 
        \y ->
            1
      |]
      <: Declaration
        { _declarationModule_ = ModuleName ["Main"],
          _declarationName = Name "x",
          _declarationBody =
            Value
              { _declarationBodyExpression =
                  Identity
                    ( Lambda
                        { arguments = [NamedPattern $ Name "y"],
                          AST.body =
                            Identity
                              ( Int 1
                              )
                        }
                    ),
                _declarationBodyPatterns = [],
                _declarationBodyTypeAnnotation = Nothing
              }
        }

testParse :: Text -> IO (Module UnwrappedExpr Pattern TypeAnnotation MaybeQualified)
testParse source = case parse "" source of
  Left err -> assertFailure (errorBundlePretty err)
  Right ast -> return (Mod.moduleDeclarations . traverse . Mod.declarationBody . declarationBodyExpression %~ unlocateExpr $ ast)


(<:) :: Text -> Declaration UnwrappedExpr Pattern TypeAnnotation MaybeQualified -> IO ()
(<:) source decl = do
  ast <- testParse source
  M.elems (ast ^. declarations) `shouldContain` [decl]

(<=>) :: Text -> Module UnwrappedExpr Pattern TypeAnnotation MaybeQualified -> IO ()
(<=>) source expected = do
  ast <- testParse source
  ast `shouldBe` expected
