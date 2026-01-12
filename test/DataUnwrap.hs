module DataUnwrap where

import Elara.AST.Region (IgnoreLocation (IgnoreLocation), Located (Located))
import Elara.Data.Unwrap (Unwrap (..))
import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Region (testRegion)
import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "Data.Unwrap" $ do
    identityTests
    locatedTests
    ignoreLocationTests

identityTests :: Spec
identityTests = describe "Unwrap Identity" $ do
    it "unwraps Identity values" $ property $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        unwrap (Identity x) === x

    it "unwraps Identity with different types" $ property $ do
        x <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        unwrap (Identity x) === x

locatedTests :: Spec
locatedTests = describe "Unwrap Located" $ do
    it "unwraps Located values" $ property $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        unwrap (Located testRegion x) === x

    it "unwraps Located with different types" $ property $ do
        x <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        unwrap (Located testRegion x) === x

ignoreLocationTests :: Spec
ignoreLocationTests = describe "Unwrap IgnoreLocation" $ do
    it "unwraps IgnoreLocation values" $ property $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        unwrap (IgnoreLocation (Located testRegion x)) === x

    it "unwraps IgnoreLocation with different types" $ property $ do
        x <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        unwrap (IgnoreLocation (Located testRegion x)) === x
