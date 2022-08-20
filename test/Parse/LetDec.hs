module Parse.LetDec where

import Control.Lens ((%~), (^.))
import Control.Lens.Plated (transformOn)
import Data.Map qualified as M
import Data.Text
import Elara.AST.Frontend as AST
import Elara.Data.Located
import Elara.Data.Module as Mod
import Elara.Data.Name
import Elara.Data.Type
import Elara.Parse (parse)
import NeatInterpolation
import Parse.Common
import System.Exit
import Test.HUnit.Lang
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Text.Megaparsec (errorBundlePretty)

testModuleName :: ModuleName
testModuleName = ModuleName ("Main" :| [])

spec :: Spec
spec = describe "Test Let Dec Parser" $ do
  it "Parses a simple let declaration" $ do
    "let x = 1"
      <: Declaration
        { _declarationModule_ = testModuleName,
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
        { _declarationModule_ = testModuleName,
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
        { _declarationModule_ = testModuleName,
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
        { _declarationModule_ = testModuleName,
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
        { _declarationModule_ = testModuleName,
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
        { _declarationModule_ = testModuleName,
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