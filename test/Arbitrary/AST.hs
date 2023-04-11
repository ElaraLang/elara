{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary.AST where

import Data.List.NonEmpty (appendList)
import Data.Set qualified as Set
import Elara.AST.Unlocated.Frontend as Unlocated
import Elara.Parse.Expression (reservedWords)
import Test.QuickCheck
import Prelude hiding (Op)
import Arbitrary.Names


instance Arbitrary Unlocated.Pattern where
    arbitrary = sized pattern'
      where
        pattern' n = if n <= 0 then nonRecursivePattern else oneof [nonRecursivePattern, recursivePattern (n `div` 2)]
        nonRecursivePattern =
            oneof
                [ VarPattern <$> arbitrary
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