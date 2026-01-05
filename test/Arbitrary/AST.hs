{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary.AST where

import Arbitrary.Literals
import Arbitrary.Names
import Elara.AST.Generic
import Elara.AST.Name (VarOrConName (VarName))
import Elara.AST.Select
import Elara.AST.Unlocated ()
import Elara.Utils
import Hedgehog hiding (Var)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude hiding (Op)

mkPat :: Pattern' UnlocatedFrontend -> Pattern UnlocatedFrontend
mkPat p = Pattern (p, Nothing)

mkExpr :: Applicative m => Expr' UnlocatedFrontend -> m (Expr UnlocatedFrontend)
mkExpr e = pure (Expr (e, Nothing))

genPattern :: Gen (Pattern UnlocatedFrontend)
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
        [ Gen.subterm2 genPattern genPattern (\x y -> mkPat (ConsPattern (x, y)))
        , mkPat . ListPattern <$> Gen.list (Range.linear 0 5) genPattern
        , (\x y -> mkPat (ConstructorPattern x y))
            <$> genMaybeQualified genTypeName
            <*> Gen.list (Range.linear 0 5) genPattern
        ]

genBinaryOperator :: Gen (BinaryOperator UnlocatedFrontend)
genBinaryOperator =
    MkBinaryOperator
        <$> Gen.choice
            [ SymOp <$> genMaybeQualified genOpName
            , Infixed <$> genMaybeQualified (VarName <$> genNormalVarName)
            ]

genExpr :: Gen (Expr UnlocatedFrontend)
genExpr = genExpr' >>= mkExpr

genExpr' :: Gen (Expr' UnlocatedFrontend)
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

genStatement' :: Gen (Expr' UnlocatedFrontend)
genStatement' =
    Gen.recursive
        Gen.choice
        [genExpr']
        [ Gen.subtermM genExpr' (\x -> Let <$> genVarName <*> Gen.list (Range.linear 0 5) genPattern <*> mkExpr x)
        , Block <$> Gen.nonEmpty (Range.linear 2 10) genExpr
        ]

genStatement :: Gen (Expr UnlocatedFrontend)
genStatement = genStatement' >>= mkExpr
