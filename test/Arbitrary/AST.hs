{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary.AST where

import Arbitrary.Names
import Elara.AST.Generic
import Elara.AST.Select
import Elara.AST.Unlocated.Frontend ()
import Test.QuickCheck
import Prelude hiding (Op)

instance Arbitrary (Pattern 'UnlocatedFrontend) where
    arbitrary :: Gen (Pattern 'UnlocatedFrontend)
    arbitrary = sized pattern'
      where
        pattern' :: Int -> Gen (Pattern 'UnlocatedFrontend)
        pattern' n = (\p -> Pattern (p, Nothing)) <$> pattern'' n

        pattern'' n = if n <= 0 then nonRecursivePattern else oneof [nonRecursivePattern, recursivePattern (n `div` 2)]

        nonRecursivePattern =
            oneof
                [ VarPattern <$> arbitraryNormalVarName
                , pure WildcardPattern
                ]

        recursivePattern n =
            oneof
                [ ConstructorPattern <$> arbitrary <*> listOf (pattern' (n `div` 2))
                , ListPattern <$> listOf (pattern' (n `div` 2))
                ]

instance Arbitrary (BinaryOperator 'UnlocatedFrontend) where
    arbitrary =
        MkBinaryOperator
            <$> oneof
                [ SymOp <$> arbitrary
                , Infixed <$> arbitrary
                ]

instance Arbitrary Text where
    arbitrary = getAlphaText <$> arbitrary

instance Arbitrary (Expr 'UnlocatedFrontend) where
    shrink (Expr (e, t)) = Expr . (,t) <$> shrink' e
      where
        shrink' (Int i) = Int <$> shrink i
        shrink' (Float f) = Float <$> shrink f
        shrink' (String s) = String <$> shrink s
        shrink' (Char c) = Char <$> shrink c
        shrink' (Var v) = Var <$> shrink v
        shrink' (Constructor c) = Constructor <$> shrink c
        shrink' (Lambda ps e) = Lambda <$> shrink ps <*> shrink e
        shrink' (FunctionCall e1 e2) = FunctionCall <$> shrink e1 <*> shrink e2
        shrink' (If e1 e2 e3) = If <$> shrink e1 <*> shrink e2 <*> shrink e3
        shrink' (BinaryOperator op e1 e2) = BinaryOperator <$> shrink op <*> shrink e1 <*> shrink e2
        shrink' (List es) = List <$> shrink es
        shrink' (LetIn v ps e1 e2) = LetIn <$> shrink v <*> shrink ps <*> shrink e1 <*> shrink e2
        shrink' (Let v ps e) = Let <$> shrink v <*> shrink ps <*> shrink e
        shrink' (Block es) = Block <$> shrink es
        shrink' (InParens e) = InParens <$> shrink e
        shrink' Unit = []
        shrink' (Tuple ts) = Tuple <$> shrink ts
        shrink' (Match e ps) = Match <$> shrink e <*> shrink ps

    arbitrary = sized expr'
      where
        expr' :: Int -> Gen (Expr 'UnlocatedFrontend)
        expr' n = wrapInExpr <$> expr'' n

        wrapInExpr :: Expr' 'UnlocatedFrontend -> Expr 'UnlocatedFrontend
        wrapInExpr = Expr . (,Nothing)

        expr'' :: Int -> Gen (Expr' 'UnlocatedFrontend)
        expr'' n = if n <= 0 then nonRecursiveExpr else oneof [nonRecursiveExpr, recursiveExpr (n `div` 2)]
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

        arbitraryBlock :: Int -> Gen (Expr UnlocatedFrontend)
        arbitraryBlock n = wrapInExpr . Block <$> atLeast 2 (arbitraryWithBlockElements n)

        arbitraryBody :: Int -> Gen (Expr UnlocatedFrontend) -- Elements that are allowed as the body of a let or lambda
        arbitraryBody n = oneof [expr' n, arbitraryBlock n]

        arbitraryWithBlockElements :: Int -> Gen (Expr 'UnlocatedFrontend) -- Elements that are allowed in a block, but not alone
        arbitraryWithBlockElements n = wrapInExpr <$> oneof [expr'' n, Let <$> arbitrary <*> sizedList arbitrary <*> arbitraryBody n]

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
