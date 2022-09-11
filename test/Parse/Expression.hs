module Parse.Expression where

import Elara.AST.Frontend
import Elara.Data.Located (TypeIdentity (TypeIdentity))
import Elara.Parse.Expression (expression)
import NeatInterpolation (text)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (eof, runParser)

spec :: Spec
spec = describe "Test Expression Parser" $ do
  it "Parses integer literals" $ do
    "3" <=> TypeIdentity (Int 3)
    "-1" <=> TypeIdentity (Int (-1))
    "0" <=> TypeIdentity (Int 0)
    "1234567890" <=> TypeIdentity (Int 1234567890)
  it "Parses float literals" $ do
    "3.14" <=> TypeIdentity (Float 3.14)
    "-1.0" <=> TypeIdentity (Float (-1.0))
    "0.0" <=> TypeIdentity (Float 0.0)
    "-0.0" <=> TypeIdentity (Float (-0.0))
    "1234567890.1234567890" <=> TypeIdentity (Float 1234567890.1234567890)
  it "Parses string literals" $ do
    [text| "Hello, World!" |] <=> TypeIdentity (String "Hello, World!")
    [text| "Hello, \"World!\"" |] <=> TypeIdentity (String "Hello, \"World!\"")
    [text| "" |] <=> TypeIdentity (String "")

  describe "Parses let .. in expressions" $ do
    let result = TypeIdentity (LetIn "x" [] (TypeIdentity (Int 1)) (TypeIdentity (Var "x")))
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

(<=>) :: Text -> UnwrappedExpr -> IO ()
(<=>) source expected = do
  let parsed = unlocateExpr <$> runParser (expression <* eof) "" source
  shouldParse parsed expected
