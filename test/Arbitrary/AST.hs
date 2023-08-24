{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary.AST where

import Arbitrary.Names
import Elara.AST.Generic
import Elara.AST.Select
import Elara.AST.Unlocated ()

import Arbitrary.Literals
import Elara.AST.Name (VarOrConName (VarName))
import Elara.Utils
import Hedgehog hiding (Var)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude hiding (Op)

mkPat :: Pattern' 'UnlocatedFrontend -> Pattern 'UnlocatedFrontend
mkPat p = Pattern (p, Nothing)

mkExpr :: (Applicative m) => Expr' 'UnlocatedFrontend -> m (Expr 'UnlocatedFrontend)
mkExpr e = pure (Expr (e, Nothing))

genPattern :: Gen (Pattern 'UnlocatedFrontend)
genPattern =
    Gen.recursive
        Gen.choice
        [ mkPat . VarPattern <$> genNormalVarName
        , pure (mkPat WildcardPattern)
        , pure (mkPat UnitPattern)
        , mkPat . IntegerPattern <$> genInteger
        , mkPat . FloatPattern <$> genDouble
        , mkPat . StringPattern <$> genLowerAlphaText
        , mkPat . CharPattern <$> Gen.unicode
        ]
        [ Gen.subterm2 genPattern genPattern (\x y -> mkPat (ConsPattern x y))
        , mkPat . ListPattern <$> Gen.list (Range.linear 0 5) genPattern
        , (\x y -> mkPat (ConstructorPattern x y)) <$> genMaybeQualified genTypeName <*> Gen.list (Range.linear 0 5) genPattern
        ]

genBinaryOperator :: Gen (BinaryOperator 'UnlocatedFrontend)
genBinaryOperator =
    MkBinaryOperator
        <$> Gen.choice
            [ SymOp <$> genMaybeQualified genOpName
            , Infixed <$> genMaybeQualified (VarName <$> genNormalVarName)
            ]

genExpr :: Gen (Expr 'UnlocatedFrontend)
genExpr = genExpr' >>= mkExpr

genExpr' :: Gen (Expr' 'UnlocatedFrontend)
genExpr' =
    Gen.recursive
        Gen.choice
        [ Int <$> genInteger
        , Float <$> genDouble
        , String <$> genLowerAlphaText
        , Char <$> Gen.unicode
        , Var <$> genMaybeQualified genVarName
        , Constructor <$> genMaybeQualified genTypeName
        ]
        [ Gen.subtermM genExpr' (\x -> Lambda <$> Gen.list (Range.linear 0 3) genPattern <*> mkExpr x)
        , Gen.subtermM2 genExpr' genExpr' (\x y -> FunctionCall <$> mkExpr x <*> mkExpr y)
        , Gen.subtermM3 genExpr' genExpr' genExpr' (\x y z -> If <$> mkExpr x <*> mkExpr y <*> mkExpr z)
        , Gen.subtermM2 genExpr' genExpr' (\y z -> curry3 BinaryOperator <$> genBinaryOperator <*> mkExpr y <*> mkExpr z)
        , List <$> Gen.list (Range.linear 0 10) genExpr
        , Gen.subtermM genExpr' (\x -> Match <$> mkExpr x <*> Gen.list (Range.linear 0 5) (liftA2 (,) genPattern genExpr))
        , LetIn <$> genVarName <*> Gen.list (Range.linear 0 5) genPattern <*> genStatement <*> genStatement
        , Tuple <$> Gen.nonEmpty (Range.linear 2 10) genExpr
        ]

genStatement' :: Gen (Expr' 'UnlocatedFrontend)
genStatement' =
    Gen.recursive
        Gen.choice
        [genExpr']
        [ Gen.subtermM genExpr' (\x -> Let <$> genVarName <*> Gen.list (Range.linear 0 5) genPattern <*> mkExpr x)
        , Block <$> Gen.nonEmpty (Range.linear 2 10) genExpr
        ]

genStatement :: Gen (Expr 'UnlocatedFrontend)
genStatement = genStatement' >>= mkExpr

-- instance Arbitrary (Expr 'UnlocatedFrontend) where
--     shrink (Expr (e, t)) = Expr . (,t) <$> shrink' e
--       where
--         shrink' (Int i) = Int <$> shrink i
--         shrink' (Float f) = Float <$> shrink f
--         shrink' (String s) = String <$> shrink s
--         shrink' (Char c) = Char <$> shrink c
--         shrink' (Var v) = Var <$> shrink v
--         shrink' (Constructor c) = Constructor <$> shrink c
--         shrink' (Lambda ps e) = Lambda <$> shrink ps <*> shrink e
--         shrink' (FunctionCall e1 e2) = FunctionCall <$> shrink e1 <*> shrink e2
--         shrink' (If e1 e2 e3) = If <$> shrink e1 <*> shrink e2 <*> shrink e3
--         shrink' (BinaryOperator op e1 e2) = BinaryOperator <$> shrink op <*> shrink e1 <*> shrink e2
--         shrink' (List es) = List <$> shrink es
--         shrink' (LetIn v ps e1 e2) = LetIn <$> shrink v <*> shrink ps <*> shrink e1 <*> shrink e2
--         shrink' (Let v ps e) = Let <$> shrink v <*> shrink ps <*> shrink e
--         shrink' (Block es) = Block <$> shrink es
--         shrink' (InParens e) = InParens <$> shrink e
--         shrink' Unit = []
--         shrink' (Tuple ts) = Tuple <$> shrink ts
--         shrink' (Match e ps) = Match <$> shrink e <*> shrink ps

--     arbitrary = sized expr'
--       where
--         expr' :: Int -> Gen (Expr 'UnlocatedFrontend)
--         expr' n = wrapInExpr <$> expr'' n

--         wrapInExpr :: Expr' 'UnlocatedFrontend -> Expr 'UnlocatedFrontend
--         wrapInExpr = Expr . (,Nothing)

--         expr'' :: Int -> Gen (Expr' 'UnlocatedFrontend)
--         expr'' n = if n <= 0 then nonRecursiveExpr else oneof [nonRecursiveExpr, recursiveExpr (n `div` 2)]
--         nonRecursiveExpr =
--             oneof
--                 [ Int <$> arbitrary
--                 , Float <$> arbitrary
--                 , String . getAlphaText <$> arbitrary
--                 , Char <$> arbitraryPrintableChar
--                 , pure Unit
--                 , Var <$> arbitrary
--                 , Constructor <$> arbitrary
--                 ]
--         recursiveExpr n =
--             oneof
--                 [ Lambda <$> listOf1 arbitrary <*> arbitraryBody n
--                 , FunctionCall <$> expr' (n `div` 2) <*> expr' (n `div` 2)
--                 , If <$> expr' (n `div` 3) <*> expr' (n `div` 3) <*> expr' (n `div` 3)
--                 , BinaryOperator <$> arbitrary <*> expr' (n `div` 2) <*> expr' (n `div` 2)
--                 , List <$> listOf (expr' (n `div` 2))
--                 , LetIn <$> arbitrary <*> arbitrary <*> arbitraryBody (n `div` 2) <*> arbitraryBody (n `div` 2)
--                 ]

--         arbitraryBlock :: Int -> Gen (Expr UnlocatedFrontend)
--         arbitraryBlock n = wrapInExpr . Block <$> atLeast 2 (arbitraryWithBlockElements n)

--         arbitraryBody :: Int -> Gen (Expr UnlocatedFrontend) -- Elements that are allowed as the body of a let or lambda
--         arbitraryBody n = oneof [expr' n, arbitraryBlock n]

--         arbitraryWithBlockElements :: Int -> Gen (Expr 'UnlocatedFrontend) -- Elements that are allowed in a block, but not alone
--         arbitraryWithBlockElements n = wrapInExpr <$> oneof [expr'' n, Let <$> arbitrary <*> sizedList arbitrary <*> arbitraryBody n]

-- instance (Arbitrary a) => Arbitrary (NonEmpty a) where
--     arbitrary = do
--         x <- arbitrary
--         xs <- arbitrary
--         pure (x :| xs)

-- atLeast :: Int -> Gen a -> Gen (NonEmpty a)
-- atLeast n _ | n < 1 = error "atLeast: n must be >= 1"
-- atLeast n gen = do
--     x <- gen
--     xs <- vectorOf (n - 1) gen
--     pure (x :| xs)

-- sizedList :: Gen a -> Gen [a]
-- sizedList gen = sized $ \n -> do
--     k <- choose (0, n)
--     vectorOf k gen
