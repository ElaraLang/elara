{-# OPTIONS_GHC -Wno-orphans #-}

module Parse.QuickCheck where

import Data.List.NonEmpty (appendList)
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
    arbitrary = frequency [(4, arbitraryNormalVarName), (1, arbitraryOpVarName)]
      where
        arbitraryNormalVarName = NormalVarName . getAlphaText <$> suchThat arbitrary isSafe
        isSafe (AlphaText name) = name `Set.notMember` reservedWords
        arbitraryOpVarName = OperatorVarName <$> arbitrary

instance Arbitrary TypeName where
    arbitrary = TypeName . getAlphaUpperText <$> arbitrary

instance Arbitrary OpName where
    arbitrary = OpName . getOpText <$> arbitrary

instance Arbitrary Unlocated.Pattern where
    arbitrary = sized pattern'
      where
        pattern' n = if n <= 0 then nonRecursivePattern else oneof [nonRecursivePattern, recursivePattern (n `div` 2)]
        nonRecursivePattern =
            oneof
                [ NamedPattern . getAlphaText <$> arbitrary
                , pure WildcardPattern
                ]
        recursivePattern n =
            oneof
                [ ConstructorPattern <$> arbitrary <*> listOf (pattern' (n `div` 2))
                , ListPattern <$> listOf (pattern' (n `div` 2))
                ]

instance Arbitrary Unlocated.BinaryOperator where
    arbitrary =
        oneof
            [ Op <$> arbitrary
            , Infixed <$> arbitrary
            ]

instance Arbitrary Unlocated.Expr where
    arbitrary = sized expr'
      where
        expr' :: Int -> Gen Unlocated.Expr
        expr' n = if n <= 0 then nonRecursiveExpr else oneof [nonRecursiveExpr, recursiveExpr (n `div` 2)]
        nonRecursiveExpr =
            oneof
                [ Int <$> arbitrary
                , Float <$> arbitrary
                , String . getAlphaText <$> arbitrary
                , Char <$> arbitraryPrintableChar
                , pure Unit
                , Var <$> arbitrary
                , Constructor <$> arbitrary
                ]
        recursiveExpr n =
            oneof
                [ Lambda <$> listOf1 arbitrary <*> arbitraryBody n
                , FunctionCall <$> expr' (n `div` 2) <*> expr' (n `div` 2)
                , If <$> expr' (n `div` 3) <*> expr' (n `div` 3) <*> expr' (n `div` 3)
                , BinaryOperator <$> arbitrary <*> expr' (n `div` 2) <*> expr' (n `div` 2)
                , List <$> listOf (expr' (n `div` 2))
                , LetIn <$> arbitrary <*> arbitrary <*> arbitraryBody (n `div` 2) <*> arbitraryBody (n `div` 2)
                ]

        -- arbitaryBlock :: Gen Unlocated.Expr
        arbitaryBlock n = Block <$> atLeast 2 (arbitraryWithBlockElements n)
        -- arbitraryBody :: Gen Unlocated.Expr -- Elements that are allowed as the body of a let or lambda
        arbitraryBody n = oneof [expr' n, arbitaryBlock n]
        -- arbitraryWithBlockElements :: Gen Unlocated.Expr -- Elements that are allowed in a block, but not alone
        arbitraryWithBlockElements n = oneof [expr' n, Let <$> arbitrary <*> sizedList arbitrary <*> arbitraryBody n]

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

sizedList :: Gen a -> Gen [a]
sizedList gen = sized $ \n -> do
    k <- choose (0, n)
    vectorOf k gen