module Parse where

import Parse.Expression qualified as Expression
import Parse.LetDec qualified as LetDec
import Parse.Names qualified as Names
import Test.Hspec

spec :: Spec
spec = do
  describe "Test Let Declarations Parsing" LetDec.spec
  describe "Test Expression Parsing" Expression.spec
  describe "Test Name Parsing" Names.spec