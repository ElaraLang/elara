{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary.Names where

import Data.Set qualified as Set
import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (..), ModuleName (..), OpName (..), Qualified (Qualified), TypeName (..), Unqualified (..), VarName (..))
import Elara.Parse.Expression (reservedWords)

import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

newtype LowerAlphaText = LowerAlphaText {getAlphaText :: Text}
    deriving (Show, Eq, Ord)

genLowerAlphaText :: Gen LowerAlphaText
genLowerAlphaText =
    LowerAlphaText
        <$> Gen.filter
            (`Set.notMember` reservedWords)
            ( do
                c <- Gen.lower
                cs <- Gen.list (Range.linear 0 10) Gen.lower
                pure $ fromString (c : cs)
            )

newtype UpperAlphaText = UpperAlphaText {getUpperAlphaText :: Text}
    deriving (Show, Eq, Ord)

genUpperAlphaText :: Gen UpperAlphaText
genUpperAlphaText =
    UpperAlphaText
        <$> Gen.filter
            (`Set.notMember` reservedWords)
            ( do
                c <- Gen.upper
                cs <- Gen.list (Range.linear 0 10) Gen.lower
                pure $ fromString (c : cs)
            )

-- instance Arbitrary AlphaText where
--     arbitrary =
--         ( AlphaText . toText <$> do
--             c <- arbitraryLower
--             cs <- listOf (oneof [arbitraryLower, arbitraryLower])
--             pure (c : cs)
--         )
--             `suchThat` isSafe
--       where
--         isSafe (AlphaText name) = name `Set.notMember` reservedWords

-- newtype AlphaUpperText = AlphaUpperText {getAlphaUpperText :: Text}
--     deriving (Show, Eq, Ord)

-- instance Arbitrary AlphaUpperText where
--     arbitrary =
--         AlphaUpperText . toText <$> do
--             c <- arbitraryUpper
--             cs <- listOf (oneof [arbitraryLower, arbitraryLower])
--             pure (c : cs)

-- arbitraryLower :: Gen Char
-- arbitraryLower = elements ['a' .. 'z']

-- arbitraryUpper :: Gen Char
-- arbitraryUpper = elements ['A' .. 'Z']

-- newtype OpText = OpText {getOpText :: Text}
--     deriving (Show, Eq, Ord)

-- instance Arbitrary OpText where
--     arbitrary =
--         OpText . toText
--             <$> listOf1 (elements ['!', '#', '$', '%', '&', '*', '+', '.', '/', '\\', '<', '>', '=', '?', '@', '^', '|', '-', '~'])
--                 `suchThat` (`Set.notMember` ["@", "=", ".", "\\", "=>", "->", "<-", "|"])
--                 `suchThat` (not . isPrefixOf "--")

-- instance Arbitrary ModuleName where
--     arbitrary = ModuleName . fromList <$> listOf1 (getAlphaUpperText <$> arbitrary)

-- instance Arbitrary name => Arbitrary (MaybeQualified name) where
--     arbitrary = do
--         qual <- oneof [pure Nothing, Just <$> arbitrary]
--         name <- arbitrary
--         pure (MaybeQualified name qual)

-- instance Arbitrary name => Arbitrary (Qualified name) where
--     arbitrary = Qualified <$> arbitrary <*> arbitrary

-- instance Arbitrary name => Arbitrary (Unqualified name) where
--     arbitrary = Unqualified <$> arbitrary

-- instance Arbitrary VarName where
--     arbitrary = frequency [(4, arbitraryNormalVarName), (1, arbitraryOpVarName)]

genNormalVarName :: Gen VarName
genNormalVarName = NormalVarName . LowerAlphaName . getAlphaText <$> genLowerAlphaText

genTypeName :: Gen TypeName
genTypeName = TypeName . getUpperAlphaText <$> genUpperAlphaText

genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> Gen.nonEmpty (Range.linear 1 3) (getUpperAlphaText <$> genUpperAlphaText)

genMaybeQualified :: Gen a -> Gen (MaybeQualified a)
genMaybeQualified gen = do
    qual <- Gen.choice [pure Nothing, Just <$> genModuleName]
    name <- gen
    pure (MaybeQualified name qual)

-- arbitraryOpVarName = OperatorVarName <$> arbitrary

-- instance Arbitrary TypeName where
--     arbitrary = TypeName . getAlphaUpperText <$> arbitrary

-- instance Arbitrary OpName where
--     arbitrary = OpName . getOpText <$> arbitrary
