module Parse where

import Orphans ()
import Parse.Expressions qualified as Expressions
import Parse.Patterns qualified as Patterns
import Test.Hspec

spec :: Spec
spec = parallel $ do
    Patterns.spec
    Expressions.spec
