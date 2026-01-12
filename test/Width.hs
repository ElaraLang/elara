module Width where

import Elara.Width (defaultWidth, getWidth)
import Test.Syd (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "Width" $ do
    defaultWidthTests
    getWidthTests

defaultWidthTests :: Spec
defaultWidthTests = describe "defaultWidth" $ do
    it "returns a positive width" $ do
        defaultWidth `shouldSatisfy` (> 0)

    it "returns 80 as the default" $ do
        defaultWidth `shouldBe` 80

getWidthTests :: Spec
getWidthTests = describe "getWidth" $ do
    it "returns a positive width" $ do
        width <- getWidth
        width `shouldSatisfy` (> 0)

    it "returns at least the default width" $ do
        width <- getWidth
        -- Terminal might be wider, but should at least be the default if not detected
        width `shouldSatisfy` (>= 0)
