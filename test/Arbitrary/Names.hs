{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedLists #-}

module Arbitrary.Names where

import Data.Set qualified as Set
import Elara.AST.Name (MaybeQualified (..), ModuleName (..), OpName (..), Qualified (Qualified), TypeName (..), Unqualified (..), VarName (..), nameText, LowerAlphaName(..))
import Elara.Parse.Expression (reservedWords)
import Test.QuickCheck

newtype AlphaText = AlphaText {getAlphaText :: Text}
    deriving (Show, Eq, Ord)

instance Arbitrary AlphaText where
    arbitrary =
        ( AlphaText . toText <$> do
            c <- arbitraryLower
            tail <- listOf (oneof [arbitraryLower, arbitraryLower])
            pure (c : tail)
        )
            `suchThat` isSafe
      where
        isSafe (AlphaText name) = name `Set.notMember` reservedWords

newtype AlphaUpperText = AlphaUpperText {getAlphaUpperText :: Text}
    deriving (Show, Eq, Ord)

instance Arbitrary AlphaUpperText where
    arbitrary =
        AlphaUpperText . toText <$> do
            c <- arbitraryUpper
            tail <- listOf (oneof [arbitraryLower, arbitraryLower])
            pure (c : tail)
arbitraryLower :: Gen Char
arbitraryLower = elements ['a' .. 'z']

arbitraryUpper :: Gen Char
arbitraryUpper = elements ['A' .. 'Z']

newtype OpText = OpText {getOpText :: Text}
    deriving (Show, Eq, Ord)

instance Arbitrary OpText where
    arbitrary =
        OpText . toText
            <$> listOf1 (elements ['!', '#', '$', '%', '&', '*', '+', '.', '/', '\\', '<', '>', '=', '?', '@', '^', '|', '-', '~'])
                `suchThat` (`Set.notMember` ["@", "=", ".", "\\", "=>", "->", "<-", "|"])
                `suchThat` (not . isPrefixOf "--")

instance Arbitrary ModuleName where
    arbitrary = ModuleName . fromList <$> listOf1 (getAlphaUpperText <$> arbitrary)

instance Arbitrary name => Arbitrary (MaybeQualified name) where
    arbitrary = do
        qual <- oneof [pure Nothing, Just <$> arbitrary]
        name <- arbitrary
        pure (MaybeQualified name qual)

instance Arbitrary name => Arbitrary (Qualified name) where
    arbitrary = Qualified <$> arbitrary <*> arbitrary

instance Arbitrary name => Arbitrary (Unqualified name) where
    arbitrary = Unqualified <$> arbitrary

instance Arbitrary VarName where
    arbitrary = frequency [(4, arbitraryNormalVarName), (1, arbitraryOpVarName)]
      where
        arbitraryNormalVarName = NormalVarName . LowerAlphaName . getAlphaText <$> arbitrary
        arbitraryOpVarName = OperatorVarName <$> arbitrary

instance Arbitrary TypeName where
    arbitrary = TypeName . getAlphaUpperText <$> arbitrary

instance Arbitrary OpName where
    arbitrary = OpName . getOpText <$> arbitrary
