{- | Type unification tests.

These tests verify type equality and basic type construction.
Direct unit testing of unify requires the full query system, so we test
the type representation and equality instead.
-}
module Infer.Unify (spec) where

import Elara.Prim (KnownType (..), KnownTypeInfo (..), OpaquePrim (..), knownTypeInfo)
import Elara.TypeInfer.Type (Monotype (..))
import Region (testRegion)
import Test.Syd

spec :: Spec
spec = describe "Type unification" $ do
    it "identical primitive types are equal" $ do
        let intType1 = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) []
        let intType2 = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) []
        intType1 `shouldBe` intType2

    it "different primitive types are not equal" $ do
        let intType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) []
        let stringType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimString))) []
        intType `shouldNotBe` stringType

    it "identical function types are equal" $ do
        let intType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) []
        let fnType1 = Function testRegion intType intType
        let fnType2 = Function testRegion intType intType
        fnType1 `shouldBe` fnType2

    it "function types with different argument types are not equal" $ do
        let intType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) []
        let stringType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimString))) []
        let fnType1 = Function testRegion intType intType
        let fnType2 = Function testRegion stringType intType
        fnType1 `shouldNotBe` fnType2

    it "function types with different return types are not equal" $ do
        let intType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) []
        let stringType = TypeConstructor testRegion (knownQualified (knownTypeInfo (KnownOpaque PrimString))) []
        let fnType1 = Function testRegion intType intType
        let fnType2 = Function testRegion intType stringType
        fnType1 `shouldNotBe` fnType2
