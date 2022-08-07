module Parse where

import Parse.LetDec qualified as LetDec
import Test.Hspec

spec :: Spec
spec = do
  describe "Test Let Declarations" LetDec.spec