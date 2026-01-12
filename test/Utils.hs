module Utils where

import Elara.Utils (curry3, uncurry3)
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "Utils" $ do
    curry3Tests
    uncurry3Tests

curry3Tests :: Spec
curry3Tests = describe "curry3" $ do
    it "converts a function taking a 3-tuple to a curried function" $ property $ do
        a <- forAll $ Gen.int (Range.linear 0 100)
        b <- forAll $ Gen.int (Range.linear 0 100)
        c <- forAll $ Gen.int (Range.linear 0 100)

        let f (x, y, z) = x + y + z
        let curried = curry3 f

        curried a b c === f (a, b, c)

    it "works with different types" $ property $ do
        a <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        b <- forAll $ Gen.bool
        c <- forAll $ Gen.int (Range.linear 0 100)

        let f (x, y, z) = (x, y, z)
        let curried = curry3 f

        curried a b c === f (a, b, c)

uncurry3Tests :: Spec
uncurry3Tests = describe "uncurry3" $ do
    it "converts a curried function to a function taking a 3-tuple" $ property $ do
        a <- forAll $ Gen.int (Range.linear 0 100)
        b <- forAll $ Gen.int (Range.linear 0 100)
        c <- forAll $ Gen.int (Range.linear 0 100)

        let f x y z = x + y + z
        let uncurried = uncurry3 f

        uncurried (a, b, c) === f a b c

    it "works with different types" $ property $ do
        a <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        b <- forAll $ Gen.bool
        c <- forAll $ Gen.int (Range.linear 0 100)

        let f x y z = (x, y, z)
        let uncurried = uncurry3 f

        uncurried (a, b, c) === f a b c

    it "curry3 and uncurry3 are inverses" $ property $ do
        a <- forAll $ Gen.int (Range.linear 0 100)
        b <- forAll $ Gen.int (Range.linear 0 100)
        c <- forAll $ Gen.int (Range.linear 0 100)

        let f x y z = x * y + z
        let roundtrip = curry3 (uncurry3 f)

        roundtrip a b c === f a b c
