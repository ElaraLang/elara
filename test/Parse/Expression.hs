module Parse.Expression where

import Elara.AST.Frontend.Pretty.Unlocated
import Elara.AST.Frontend.Unlocated as Unlocated
import Elara.AST.Name
import Elara.AST.Region (unlocate)
import Elara.Parse.Expression (exprParser, expression, operator)
import Elara.Parse.Primitives (Parser)
import NeatInterpolation (text)
import Parse.Common (shouldParseProp)
import Parse.Orphans ()
import Parse.QuickCheck
import Print (printColored)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Test.Hspec.QuickCheck
import Test.QuickCheck (ASCIIString (ASCIIString), forAll, generate)
import Test.QuickCheck.Property
import Text.Megaparsec (ParseErrorBundle, eof, runParser)
import Prelude hiding (Op)

spec :: Spec
spec = describe "Test Expression Parser" $ do
  literals
  operators
  letIn
  quickCheck

literals :: Spec
literals = describe "Parses literals" $ do
  it "Parses integer literals" $ do
    "3" <=> (Int 3)
    "-1" <=> (Int (-1))
    "0" <=> (Int 0)
    "1234567890" <=> (Int 1234567890)
  it "Parses float literals" $ do
    "3.14" <=> (Float 3.14)
    "-1.0" <=> (Float (-1.0))
    "0.0" <=> (Float 0.0)
    "-0.0" <=> (Float (-0.0))
    "1234567890.1234567890" <=> (Float 1234567890.1234567890)
  it "Parses string literals" $ do
    [text| "Hello, World!" |] <=> (String "Hello, World!")
    [text| "Hello, \"World!\"" |] <=> (String "Hello, \"World!\"")
    [text| "" |] <=> (String "")

operators :: Spec
operators = describe "Parses operators" $ do
  describe "Parses standalone operator symbols" $ do
    let
      prop_InfixedParses :: Text -> Property
      prop_InfixedParses str = shouldParseProp (stripLocation <$> parse operator ("`" <> str <> "`")) (Infixed (MaybeQualified (VarName str) Nothing))
      prop_OpParses :: Text -> Property
      prop_OpParses str = shouldParseProp (stripLocation <$> parse operator str) (Op (MaybeQualified (OpName str) Nothing))

    prop "Parses Infix operators" (prop_InfixedParses . getAlphaText)
    prop "Infix operators don't parse as ASCII operators" (expectFailure . prop_OpParses . getAlphaText)
    prop "Parses ASCII operators" (prop_OpParses . getOpText)
    prop "ASCII operators don't parse as Infix operators" (expectFailure . prop_InfixedParses . getOpText)

letIn :: Spec
letIn = describe "Parses let-in expressions" $ do
  describe "Parses let .. in expressions" $ do
    let result = (LetIn "x" [] ((Int 1)) ((Var "x")))
    it "Parses on the same line " $ do
      [text| let x = 1 in x |] <=> result
    it "Parses multi line 1" $ do
      [text| 
      let x = 
           1
       in x |]
        <=> result
    it "Parses multi line 2" $ do
      [text|
      let x = 
           1 
       in 
        x |]
        <=> result
    it "Parses multi line 3" $ do
      [text| 
     let x = 1 in
      x |]
        <=> result
    it "Parses multi line 4" $ do
      [text| 
     let x = 1
       in 
      x |]
        <=> result

    it "Parses multi line 5" $ do
      [text| 
      let x =
            1 in x
      |]
        <=> result

quickCheck :: Spec
quickCheck = prop "Arbitrary expressions parse prettyPrinted" ppEq

ppEq :: Expr -> Property
ppEq expr =
  let
    source = prettyPrint expr
    parsed = stripLocation <$> parse exprParser source
   in
    whenFail' (printColored (expr, source, parsed)) (parsed `shouldParseProp` expr)

parse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parse p = runParser (p <* eof) ""

(<=>) :: Text -> Unlocated.Expr -> IO ()
(<=>) source expected = do
  let parsed = stripLocation <$> parse exprParser source
  shouldParse parsed expected