module ASTRegion where

import Elara.AST.Region (
    GeneratedRegion,
    IgnoreLocation (..),
    Located (..),
    RealSourceRegion,
    SourceRegion (..),
    enclosingRegion',
    generatedLocated,
    generatedSourceRegion,
    spanningRegion',
    unlocated,
    withLocationOf,
 )
import Region (testRegion)
import Test.Syd (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "AST.Region" $ do
    locatedTests
    generatedRegionTests
    ignoreLocationTests
    regionCombinationTests

locatedTests :: Spec
locatedTests = describe "Located" $ do
    it "stores a value with a source region" $ do
        let loc = Located testRegion (42 :: Int)
        loc `shouldSatisfy` \(Located _ x) -> x == 42

    it "has a lens to access the unlocated value" $ do
        let loc = Located testRegion (42 :: Int)
        let val = loc ^. unlocated
        val `shouldBe` 42

    it "can modify the unlocated value" $ do
        let loc = Located testRegion (42 :: Int)
        let modified = loc & unlocated %~ (* 2)
        modified ^. unlocated `shouldBe` 84

    it "withLocationOf copies location from one Located to another" $ do
        let loc1 = Located testRegion (42 :: Int)
        let loc2 = withLocationOf "test" loc1
        loc2 `shouldSatisfy` \(Located _ x) -> x == "test"

    it "Applicative instance combines regions" $ do
        let f = Located testRegion ((+ 1) :: Int -> Int)
        let x = Located testRegion (41 :: Int)
        let result = f <*> x
        result ^. unlocated `shouldBe` 42

    it "pure creates Located with generated region" $ do
        let loc = pure (42 :: Int) :: Located Int
        loc ^. unlocated `shouldBe` 42

generatedRegionTests :: Spec
generatedRegionTests = describe "Generated regions" $ do
    it "creates a generated source region" $ do
        let region = generatedSourceRegion (Just "test.elr")
        region `shouldSatisfy` const True

    it "creates generated located values" $ do
        let loc = generatedLocated (Just "test.elr") (42 :: Int)
        loc ^. unlocated `shouldBe` 42

ignoreLocationTests :: Spec
ignoreLocationTests = describe "IgnoreLocation" $ do
    it "wraps Located and ignores location in equality" $ do
        let loc1 = IgnoreLocation (Located testRegion (42 :: Int))
        let loc2 = IgnoreLocation (Located (generatedSourceRegion Nothing) (42 :: Int))
        loc1 `shouldBe` loc2

    it "distinguishes different values" $ do
        let loc1 = IgnoreLocation (Located testRegion (42 :: Int))
        let loc2 = IgnoreLocation (Located testRegion (43 :: Int))
        loc1 `shouldSatisfy` (/= loc2)

regionCombinationTests :: Spec
regionCombinationTests = describe "Region combination" $ do
    it "enclosingRegion' combines two regions" $ do
        let r1 = generatedSourceRegion (Just "test.elr")
        let r2 = generatedSourceRegion (Just "test.elr")
        let combined = enclosingRegion' r1 r2
        combined `shouldSatisfy` const True

    it "spanningRegion' combines multiple regions" $ do
        let r1 = generatedSourceRegion (Just "test.elr")
        let r2 = generatedSourceRegion (Just "test.elr")
        let r3 = generatedSourceRegion (Just "test.elr")
        let spanning = spanningRegion' (r1 :| [r2, r3])
        spanning `shouldSatisfy` const True

    it "Semigroup instance for SourceRegion" $ do
        let r1 = generatedSourceRegion (Just "test.elr")
        let r2 = generatedSourceRegion (Just "test.elr")
        let combined = r1 <> r2
        combined `shouldSatisfy` const True

    it "Monoid instance provides mempty" $ do
        let empty = mempty :: SourceRegion
        empty `shouldSatisfy` const True
