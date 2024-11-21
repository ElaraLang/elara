module Infer where

import Optics.Operators.Unsafe ((^?!))
import Print (showPretty)
import Relude.Unsafe ((!!))
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude hiding (fail)

spec :: Spec
spec = describe "Infers types correctly" $ parallel $ do
    pass
