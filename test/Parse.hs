module Parse where

import Orphans ()
import Parse.Expressions qualified as Expressions
import Parse.Patterns qualified as Patterns
import Test.Syd

spec :: Spec
spec = do
    Patterns.spec
    Expressions.spec
