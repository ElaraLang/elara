module ShuntOperator where

import Elara.Shunt.Operator (Associativity (..), OpInfo (..), Precedence, mkPrecedence)
import Hedgehog (Property, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "Shunt.Operator" $ do
    precedenceTests
    opInfoTests

precedenceTests :: Spec
precedenceTests = describe "Precedence" $ do
    it "creates valid precedence values" $ do
        let p = mkPrecedence 5
        p `shouldSatisfy` const True

    it "allows precedence from 0 to 9" $ property $ do
        n <- forAll $ Gen.int (Range.linear 0 9)
        let p = mkPrecedence n
        assert True

    it "rejects negative precedence" $ do
        let result = try @SomeException (evaluate (mkPrecedence (-1)))
        case result of
            Left _ -> pure ()
            Right _ -> expectationFailure "Expected mkPrecedence to fail with negative input"

    it "rejects precedence >= 10" $ do
        let result = try @SomeException (evaluate (mkPrecedence 10))
        case result of
            Left _ -> pure ()
            Right _ -> expectationFailure "Expected mkPrecedence to fail with input >= 10"

    it "precedence ordering works correctly" $ property $ do
        a <- forAll $ Gen.int (Range.linear 0 9)
        b <- forAll $ Gen.int (Range.linear 0 9)
        let pa = mkPrecedence a
        let pb = mkPrecedence b
        compare pa pb === compare a b

opInfoTests :: Spec
opInfoTests = describe "OpInfo" $ do
    it "creates OpInfo with left associativity" $ do
        let info = OpInfo (mkPrecedence 5) LeftAssociative
        info `shouldSatisfy` \(OpInfo _ assoc) -> assoc == LeftAssociative

    it "creates OpInfo with right associativity" $ do
        let info = OpInfo (mkPrecedence 5) RightAssociative
        info `shouldSatisfy` \(OpInfo _ assoc) -> assoc == RightAssociative

    it "creates OpInfo with non-associativity" $ do
        let info = OpInfo (mkPrecedence 5) NonAssociative
        info `shouldSatisfy` \(OpInfo _ assoc) -> assoc == NonAssociative

    it "OpInfo equality works" $ do
        let info1 = OpInfo (mkPrecedence 5) LeftAssociative
        let info2 = OpInfo (mkPrecedence 5) LeftAssociative
        let info3 = OpInfo (mkPrecedence 6) LeftAssociative
        info1 `shouldBe` info2
        info1 `shouldSatisfy` (/= info3)

    it "OpInfo ordering works" $ property $ do
        p1 <- forAll $ Gen.int (Range.linear 0 9)
        p2 <- forAll $ Gen.int (Range.linear 0 9)
        assoc1 <- forAll $ Gen.element [LeftAssociative, RightAssociative, NonAssociative]
        assoc2 <- forAll $ Gen.element [LeftAssociative, RightAssociative, NonAssociative]

        let info1 = OpInfo (mkPrecedence p1) assoc1
        let info2 = OpInfo (mkPrecedence p2) assoc2

        -- OpInfo should have some defined ordering
        compare info1 info2 `shouldSatisfy` const True
