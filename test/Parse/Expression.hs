{-# LANGUAGE ViewPatterns #-}

module Parse.Expression where

import Elara.AST.Frontend.Pretty.Unlocated
import Elara.AST.Frontend.StripLocation
import Elara.AST.Frontend.Unlocated as Unlocated
import Elara.AST.Name
import Elara.Parse.Error
import Elara.Parse.Expression (exprParser, operator)
import Elara.Parse.Primitives (HParser, toParsec)
import NeatInterpolation (text)
import Orphans ()
import Parse.Common (shouldParseProp)
import Parse.QuickCheck
import Print (printColored)
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Megaparsec (shouldParse)
import Test.Hspec.QuickCheck
import Test.QuickCheck (verboseCheck)
import Test.QuickCheck.Property
import Text.Megaparsec (ParseErrorBundle, eof, runParser)
import Prelude hiding (Op)

spec :: Spec
spec = describe "Test Expression Parser" $ do
  literals
  operators
  letIn
  ifElse
  quickCheck

literals :: Spec
literals = describe "Parses literals" $ do
  it "Parses integer literals" $ do
    "3" <=> Int 3
    "-1" <=> Int (-1)
    "0" <=> Int 0
    "1234567890" <=> Int 1234567890
  it "Parses float literals" $ do
    "3.14" <=> Float 3.14
    "-1.0" <=> Float (-1.0)
    "0.0" <=> Float 0.0
    "-0.0" <=> Float (-0.0)
    "1234567890.1234567890" <=> Float 1234567890.1234567890
  it "Parses string literals" $ do
    [text| "Hello, World!" |] <=> String "Hello, World!"
    [text| "Hello, \"World!\"" |] <=> String "Hello, \"World!\""
    [text| "" |] <=> String ""

operators :: Spec
operators = describe "Parses operators" $ describe "Parses standalone operator symbols" $ do
  let
    prop_InfixedParses :: Text -> Property
    prop_InfixedParses str = shouldParseProp (stripLocation <$> parse operator ("`" <> str <> "`")) (Infixed (MaybeQualified (NormalVarName str) Nothing))
    prop_OpParses :: Text -> Property
    prop_OpParses str = shouldParseProp (stripLocation <$> parse operator str) (Op (MaybeQualified (OpName str) Nothing))

  prop "Parses Infix operators" (prop_InfixedParses . getAlphaText)
  prop "Infix operators don't parse as ASCII operators" (expectFailure . prop_OpParses . getAlphaText)
  prop "Parses ASCII operators" (prop_OpParses . getOpText)
  prop "ASCII operators don't parse as Infix operators" (expectFailure . prop_InfixedParses . getOpText)

letIn :: Spec
letIn =
  describe "Parses let .. in expressions" $ do
    let result = LetIn "x" [] (Int 1) (Var "x")
    it "Parses on the same line " $ [text| let x = 1 in x |] <=> result
    it "Parses multi line 1" $
      [text| 
      let x = 
           1
       in x |]
        <=> result
    it "Parses multi line 2" $
      [text|
      let x = 
           1 
       in 
        x |]
        <=> result
    it "Parses multi line 3" $
      [text| 
     let x = 1 in
      x |]
        <=> result
    it "Parses multi line 4" $
      [text| 
     let x = 1
       in 
      x |]
        <=> result

    it "Parses multi line 5" $
      [text| 
      let x =
            1 in x
      |]
        <=> result

ifElse :: Spec
ifElse = describe "Parses if-then-else expressions" $ do
  let result = If (Int 1) (Int 2) (Int 3)
  it "Parses on the same line" $ [text| if 1 then 2 else 3 |] <=> result
  it "Parses multi line 1" $
    [text| 
    if 1
    then 2
    else 3 |]
      <=> result
  it "Parses multi line 2" $
    [text| 
    if 1
    then 2
    else 
      3 |]
      <=> result
  it "Parses multi line 3" $
    [text| 
    if 1
    then 
      2
    else 3 |]
      <=> result
  it "Parses multi line 4" $
    [text| 
    if 1
    then 
      2
    else 
      3 |]
      <=> result
  it "Parses multi line 5" $
    [text|
    if
     1
    then
     2
    else
     3 |]
      <=> result
  it "Parses multi line 6" $
    [text|
      if 1
      then 2
      else 3
    |]
      <=> result

  it "Parses multi line 7" $
    [text|
      if 1
         then 2
         else
          3
    |]
      <=> result

quickCheck :: Spec
quickCheck = modifyMaxSize (const 5) $ fprop "Arbitrary expressions parse prettyPrinted" (\e -> verboseCheck (ppEq e))

removeInParens :: Expr -> Expr
removeInParens (Lambda p e) = Lambda p (removeInParens e)
removeInParens (FunctionCall e1 e2) = FunctionCall (removeInParens e1) (removeInParens e2)
removeInParens (If e1 e2 e3) = If (removeInParens e1) (removeInParens e2) (removeInParens e3)
removeInParens (BinaryOperator op e1 e2) = BinaryOperator op (removeInParens e1) (removeInParens e2)
removeInParens (List es) = List (removeInParens <$> es)
removeInParens (LetIn v ps e1 e2) = LetIn v ps (removeInParens e1) (removeInParens e2)
removeInParens (Let v ps e) = Let v ps (removeInParens e)
removeInParens (Block es) = Block (removeInParens <$> es)
removeInParens (InParens e) = removeInParens e
removeInParens e = e

ppEq :: Expr -> Property
ppEq (removeInParens -> expr) =
  let
    source = prettyPrint expr
    parsed = removeInParens . stripLocation <$> parse exprParser source
   in
    ioProperty (print source $> whenFail' (putTextLn source) (parsed `shouldParseProp` expr))

parse :: HParser a -> Text -> Either (ParseErrorBundle Text ElaraParseError) a
parse p = runParser (toParsec p <* eof) ""

(<=>) :: Text -> Unlocated.Expr -> IO ()
(<=>) source expected = do
  let parsed = stripLocation <$> parse exprParser source
  shouldParse parsed expected