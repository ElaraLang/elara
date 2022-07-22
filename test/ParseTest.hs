module ParseTest where

import Data.Map qualified as M
import Data.Text
import Elara.AST.Frontend as AST
import Elara.Data.Located
import Elara.Data.Module as Mod
import Elara.Data.Name
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.TypeAnnotation (TypeAnnotation)
import Elara.Parse (parse)
import NeatInterpolation
import System.Exit
import Test.HUnit.Lang
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Text.Megaparsec (errorBundlePretty)

-- These tests kinda suck because we're also having to test the AST locations, which adds a lot of boilerplate.
-- Unfortunately refactoring the AST types to be more abstracted away from the locations is a lot of work
-- so this is the best we can do for now.

spec :: Spec
spec = describe "Test Parser" $ do
  it "Parses a simple let declaration" $ do
    "let x = 1"
      <: Declaration
        { module_ = ModuleName ["Main"],
          name = Name "x",
          Mod.body =
            Value
              { expression = Located (Region 8 9) (Int 1),
                patterns = [],
                typeAnnotation = Nothing
              }
        }

  it "Parses an indented let declaration" $
    do
      [text|
      let x = 
        1
      |]
      <: Declaration
        { module_ = ModuleName ["Main"],
          name = Name "x",
          Mod.body =
            Value
              { expression =
                  Located
                    (Region 11 12)
                    ( Int 1
                    ),
                patterns = [],
                typeAnnotation = Nothing
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
        { module_ = ModuleName ["Main"],
          name = Name "x",
          Mod.body =
            Value
              { expression =
                  Located
                    (Region 11 16)
                    ( Block
                        [ Located (Region 11 12) (Int 1),
                          Located (Region 15 16) (Int 2)
                        ]
                    ),
                patterns = [],
                typeAnnotation = Nothing
              }
        }

  it "Parses a normal let with an indented lambda" $
    do
      [text|
      let x = \y ->
               1
      |]
      <: Declaration
        { module_ = ModuleName ["Main"],
          name = Name "x",
          Mod.body =
            Value
              { expression =
                  Located
                    (Region 8 24)
                    ( Lambda
                        { arguments = [Name "y"],
                          AST.body =
                            Located
                              (Region 23 24)
                              ( Int 1
                              )
                        }
                    ),
                patterns = [],
                typeAnnotation = Nothing
              }
        }
  it "Parses a normal let with a non-indented lambda" $
    do
      [text|
      let x = 
        \y -> 1
      |]
      <: Declaration
        { module_ = ModuleName ["Main"],
          name = Name "x",
          Mod.body =
            Value
              { expression =
                  Located
                    (Region 11 18)
                    ( Lambda
                        { arguments = [Name "y"],
                          AST.body =
                            Located
                              (Region 17 18)
                              ( Int 1
                              )
                        }
                    ),
                patterns = [],
                typeAnnotation = Nothing
              }
        }

testParse :: Text -> IO (Module LocatedExpr Pattern TypeAnnotation MaybeQualified)
testParse source = case parse "" source of
  Left err -> assertFailure (errorBundlePretty err)
  Right ast -> return (ast)

(<:) :: Text -> Declaration LocatedExpr Pattern TypeAnnotation MaybeQualified -> IO ()
(<:) source decl = do
  ast <- testParse source
  M.elems (_declarations ast) `shouldContain` [decl]

(<=>) :: Text -> Module LocatedExpr Pattern TypeAnnotation MaybeQualified -> IO ()
(<=>) source expected = do
  ast <- testParse source
  ast `shouldBe` expected
