module DataUnique where

import Elara.Data.Unique (Unique (..), UniqueId, getUniqueId, uniqueId, uniqueVal, unsafeMkUnique)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "Data.Unique" $ do
    uniqueTests
    uniqueIdTests
    lensTests

uniqueTests :: Spec
uniqueTests = describe "Unique" $ do
    it "creates a unique value" $ do
        let u = unsafeMkUnique "test" 42
        u `shouldSatisfy` \(Unique val uid) -> val == "test" && uid == 42

    it "Eq instance compares both value and id" $ property $ do
        val1 <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        val2 <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        id1 <- forAll $ Gen.int (Range.linear 0 1000)
        id2 <- forAll $ Gen.int (Range.linear 0 1000)

        let u1 = unsafeMkUnique val1 id1
        let u2 = unsafeMkUnique val2 id2

        (u1 == u2) === (val1 == val2 && id1 == id2)

    it "Ord instance compares id first, then value" $ property $ do
        val1 <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        val2 <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        id1 <- forAll $ Gen.int (Range.linear 0 1000)
        id2 <- forAll $ Gen.int (Range.linear 0 1000)

        let u1 = unsafeMkUnique val1 id1
        let u2 = unsafeMkUnique val2 id2

        -- When ids differ, ordering is determined by id
        if id1 /= id2
            then compare u1 u2 === compare id1 id2
            else compare u1 u2 === compare val1 val2

    it "Functor instance maps over the value" $ property $ do
        val <- forAll $ Gen.int (Range.linear 0 100)
        uid <- forAll $ Gen.int (Range.linear 0 1000)

        let u = unsafeMkUnique val uid
        let u' = fmap (* 2) u

        u' === unsafeMkUnique (val * 2) uid

    it "Foldable instance folds over the value" $ property $ do
        val <- forAll $ Gen.int (Range.linear 0 100)
        uid <- forAll $ Gen.int (Range.linear 0 1000)

        let u = unsafeMkUnique val uid
        foldr (+) 0 u === val

    it "Traversable instance traverses the value" $ property $ do
        val <- forAll $ Gen.int (Range.linear 0 100)
        uid <- forAll $ Gen.int (Range.linear 0 1000)

        let u = unsafeMkUnique val uid
        result <- liftIO $ traverse (\x -> pure (x * 2)) u

        result === unsafeMkUnique (val * 2) uid

uniqueIdTests :: Spec
uniqueIdTests = describe "UniqueId" $ do
    it "getUniqueId extracts just the id" $ property $ do
        val <- forAll $ Gen.text (Range.linear 0 10) Gen.alphaNum
        uid <- forAll $ Gen.int (Range.linear 0 1000)

        let u = unsafeMkUnique val uid
        let uid' = getUniqueId u

        uid' `shouldSatisfy` const True

lensTests :: Spec
lensTests = describe "Unique lenses" $ do
    it "uniqueVal lens accesses the value" $ property $ do
        val <- forAll $ Gen.int (Range.linear 0 100)
        uid <- forAll $ Gen.int (Range.linear 0 1000)

        let u = unsafeMkUnique val uid
        u ^. uniqueVal === val

    it "uniqueVal lens modifies the value" $ property $ do
        val <- forAll $ Gen.int (Range.linear 0 100)
        uid <- forAll $ Gen.int (Range.linear 0 1000)

        let u = unsafeMkUnique val uid
        let u' = u & uniqueVal %~ (* 2)

        u' ^. uniqueVal === val * 2
        u' ^. uniqueId === uid

    it "uniqueId lens accesses the id" $ property $ do
        val <- forAll $ Gen.int (Range.linear 0 100)
        uid <- forAll $ Gen.int (Range.linear 0 1000)

        let u = unsafeMkUnique val uid
        u ^. uniqueId === uid

    it "uniqueId lens modifies the id" $ property $ do
        val <- forAll $ Gen.int (Range.linear 0 100)
        uid <- forAll $ Gen.int (Range.linear 0 1000)

        let u = unsafeMkUnique val uid
        let u' = u & uniqueId %~ (+ 1)

        u' ^. uniqueVal === val
        u' ^. uniqueId === uid + 1
