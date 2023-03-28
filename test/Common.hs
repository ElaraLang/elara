module Common where

import Test.Hspec

(<=>) :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
(<=>) = shouldBe