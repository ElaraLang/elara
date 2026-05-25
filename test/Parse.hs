-- | Parser tests
module Parse (spec) where

import Test.Syd (Spec)

import Orphans ()

import Parse.Expressions qualified as Expressions
import Parse.Patterns qualified as Patterns

spec :: Spec
spec = do
    Patterns.spec
    Expressions.spec
