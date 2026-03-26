{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary.AST where

import Arbitrary.Literals
import Arbitrary.Names
import Elara.AST.Name (VarName (..), VarOrConName (VarName))
import Elara.AST.New.Extensions
import Elara.AST.New.Phase (NoExtension (..))
import Elara.AST.New.Phases.Frontend
import Elara.AST.New.Types
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Hedgehog hiding (Var)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Normalise (mkExpr, mkPat)
import Prelude hiding (Op)

genPattern :: Gen (Pattern () Frontend)
genPattern =
    Gen.recursive
        Gen.choice
        [ Normalise.mkPat . PVar . NormalVarName <$> genNormalVarName
        , pure (Normalise.mkPat PWildcard)
        , pure (Normalise.mkPat PUnit)
        , Normalise.mkPat . PInt <$> genInteger
        , Normalise.mkPat . PFloat <$> genDouble
        , Normalise.mkPat . PString <$> genLowerAlphaText
        , Normalise.mkPat . PChar <$> Gen.unicode
        ]
        [ Gen.subterm2 genPattern genPattern (\x y -> Normalise.mkPat (PExtension (ConsPattern x y)))
        , Normalise.mkPat . PExtension . ListPattern <$> Gen.list (Range.linear 0 5) genPattern
        , (\c ps -> Normalise.mkPat (PCon c ps))
            <$> genMaybeQualified genTypeName
            <*> Gen.list (Range.linear 0 5) genPattern
        ]

genBinaryOperator :: Gen (BinaryOperator () Frontend)
genBinaryOperator =
    Gen.choice
        [ SymOp () <$> genMaybeQualified genOpName
        , InfixedOp () . fmap VarName <$> genMaybeQualified genNormalVarName
        ]

genExpr :: Gen (Expr () Frontend)
genExpr = Normalise.mkExpr <$> genExpr'

genExpr' :: Gen (Expr' () Frontend)
genExpr' =
    Gen.recursive
        Gen.choice
        [ EInt <$> genInteger
        , EFloat <$> genDouble
        , EString <$> genLowerAlphaText
        , EChar <$> Gen.unicode
        , EVar NoExtension <$> genMaybeQualified genVarName
        , ECon NoExtension <$> genMaybeQualified genTypeName
        ]
        [ Gen.subtermM
            genExpr'
            ( \x -> do
                pats <- Gen.list (Range.linear 0 3) genPattern
                let body = Normalise.mkExpr x
                pure $ case pats of
                    [] -> EExtension (FrontendMultiLam [Normalise.mkPat PWildcard] body)
                    _ -> EExtension (FrontendMultiLam pats body)
            )
        , Gen.subtermM2 genExpr' genExpr' (\x y -> pure $ EApp NoExtension (Normalise.mkExpr x) (Normalise.mkExpr y))
        , Gen.subtermM3 genExpr' genExpr' genExpr' (\x y z -> pure $ EIf (Normalise.mkExpr x) (Normalise.mkExpr y) (Normalise.mkExpr z))
        , Gen.subtermM2
            genExpr'
            genExpr'
            ( \y z -> do
                op <- genBinaryOperator
                pure $ EExtension (FrontendBinaryOperator (BinaryOperatorExpression op (Normalise.mkExpr y) (Normalise.mkExpr z)))
            )
        , EExtension . FrontendList . ListExpression <$> Gen.list (Range.linear 0 10) genExpr
        , Gen.subtermM
            genExpr'
            ( \x -> do
                cases <- Gen.list (Range.linear 0 5) (liftA2 (,) genPattern genExpr)
                pure $ EMatch (Normalise.mkExpr x) cases
            )
        , Gen.subtermM2
            genExpr'
            genExpr'
            ( \x y -> do
                n <- genVarName
                pats <- Gen.list (Range.linear 0 5) genPattern
                let e = Normalise.mkExpr x
                let body = Normalise.mkExpr y
                pure $ case pats of
                    [] -> ELetIn NoExtension n e body
                    _ -> EExtension (FrontendLetInWithPatterns n pats e body)
            )
        , EExtension . FrontendTuple . TupleExpression . AtLeast2List.fromNonEmptyUnsafe <$> Gen.nonEmpty (Range.linear 2 10) genExpr
        ]

genStatement' :: Gen (Expr' () Frontend)
genStatement' =
    Gen.recursive
        Gen.choice
        [genExpr']
        [ Gen.subtermM
            genExpr'
            ( \x -> do
                n <- genVarName
                pats <- Gen.list (Range.linear 0 5) genPattern
                let e = Normalise.mkExpr x
                pure $ case pats of
                    [] -> ELet NoExtension n e
                    _ -> EExtension (FrontendLetWithPatterns n pats e)
            )
        , EBlock <$> Gen.nonEmpty (Range.linear 2 10) genExpr
        ]

genStatement :: Gen (Expr () Frontend)
genStatement = Normalise.mkExpr <$> genStatement'
