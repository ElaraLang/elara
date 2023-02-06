{-# OPTIONS_GHC -Wno-orphans #-}

module Parse.QuickCheck where

import Data.Set qualified as Set
import Elara.AST.Frontend.Unlocated as Unlocated
import Elara.AST.Name (MaybeQualified (..), ModuleName (..), OpName (..), Qualified (Qualified), TypeName (..), VarName (..), nameText)
import Elara.Parse.Expression (reservedWords)
import Test.QuickCheck
import Prelude hiding (Op)

newtype AlphaText = AlphaText {getAlphaText :: Text}
    deriving (Show, Eq, Ord)

instance Arbitrary AlphaText where
    arbitrary =
        AlphaText . toText <$> do
            c <- arbitraryLower
            tail <- listOf (oneof [arbitraryLower, arbitraryLower])
            pure (c : tail)

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
    arbitrary = OpText . toText <$> listOf1 (elements ['!', '#', '$', '%', '&', '*', '+', '.', '/', '\\', '<', '>', '=', '?', '@', '^', '|', '-', '~'])

instance Arbitrary ModuleName where
    arbitrary = ModuleName . fromList <$> listOf1 (getAlphaUpperText <$> arbitrary)

instance Arbitrary name => Arbitrary (MaybeQualified name) where
    arbitrary = do
        qual <- oneof [pure Nothing, Just <$> arbitrary]
        name <- arbitrary
        pure (MaybeQualified name qual)

instance Arbitrary name => Arbitrary (Qualified name) where
    arbitrary = Qualified <$> arbitrary <*> arbitrary

instance Arbitrary VarName where
    arbitrary = VarName . getAlphaText <$> suchThat arbitrary isSafe
      where
        isSafe (AlphaText name) = name `Set.notMember` reservedWords

instance Arbitrary TypeName where
    arbitrary = TypeName . getAlphaUpperText <$> arbitrary

instance Arbitrary OpName where
    arbitrary = OpName . getOpText <$> arbitrary

instance Arbitrary Unlocated.Pattern where
    arbitrary =
        oneof
            [ NamedPattern . getAlphaText <$> arbitrary
            , ConstructorPattern <$> arbitrary <*> listOf arbitrary
            , ListPattern <$> listOf arbitrary
            , pure WildcardPattern
            ]

instance Arbitrary Unlocated.BinaryOperator where
    arbitrary =
        oneof
            [ Op <$> arbitrary
            , Infixed <$> arbitrary
            ]

instance Arbitrary Unlocated.Expr where
    arbitrary =
        oneof
            [ Int <$> arbitrary
            , Float <$> arbitrary
            , String . getAlphaText <$> arbitrary
            , Char <$> arbitraryPrintableChar
            , pure Unit
            , Var <$> arbitrary
            , Constructor <$> arbitrary
            , Lambda <$> listOf1 arbitrary <*> arbitraryWithBlockElements
            , FunctionCall <$> arbitrary <*> arbitrary
            , If <$> arbitrary <*> arbitrary <*> arbitrary
            , BinaryOperator <$> arbitrary <*> arbitrary <*> arbitrary
            , List <$> listOf arbitrary
            , LetIn <$> arbitrary <*> arbitrary <*> arbitraryWithBlockElements <*> arbitraryWithBlockElements
            ]
      where
        arbitaryBlock :: Gen Unlocated.Expr
        arbitaryBlock = Block <$> atLeast 2 arbitrary -- The parser requires at least 2 elements in a block
        arbitraryWithBlockElements :: Gen Unlocated.Expr
        arbitraryWithBlockElements = oneof [arbitrary, Let <$> arbitrary <*> arbitrary <*> arbitraryWithBlockElements, arbitaryBlock]

instance (Arbitrary a) => Arbitrary (NonEmpty a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        pure (x :| xs)

atLeast :: Int -> Gen a -> Gen (NonEmpty a)
atLeast n _ | n < 1 = error "atLeast: n must be >= 1"
atLeast n gen = do
    x <- gen
    xs <- vectorOf (n - 1) gen
    pure (x :| xs)