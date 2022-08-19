module Parse where

import Parse.LetDec qualified as LetDec
import Parse.Names qualified as Names
import Test.Hspec

spec :: Spec
spec = do
  describe "Test Let Declarations" LetDec.spec
  describe "Test Name Parsing" Names.spec