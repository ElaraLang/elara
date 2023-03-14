{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elara.Annotate.Shunt where

import Control.Lens (over, (^.))
import Data.Map (lookup)
import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Name (Name (NOpName, NVarName), NameLike (fullNameText), Qualified)
import Elara.AST.Region (Located (..), SourceRegion, sourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.Error (ReportableError (..))
import Elara.Error.Codes qualified as Codes
import Elara.Error.Effect (writeReport)
import Error.Diagnose
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.Writer
import Prelude hiding (State, execState, gets, modify')

type OpTable = Map (Qualified Name) OpInfo

newtype Precedence = Precedence Int
    deriving (Show, Eq, Ord)

mkPrecedence :: Int -> Precedence
mkPrecedence i
    | i < 0 = error "Precedence must be positive"
    | i > 9 = error "Precedence must be less than 10"
    | otherwise = Precedence i

data OpInfo = OpInfo
    { precedence :: Precedence
    , associativity :: Associativity
    }
    deriving (Show, Eq)

data Associativity
    = LeftAssociative
    | RightAssociative
    | NonAssociative
    deriving (Show, Eq)

data ShuntError
    = SamePrecedenceError (Located (Qualified Name), Associativity) (Located (Qualified Name), Associativity)
    deriving (Show, Eq)

instance ReportableError ShuntError where
    report (SamePrecedenceError (op1, a1) (op2, a2)) = do
        let op1Src = sourceRegionToDiagnosePosition $ op1 ^. sourceRegion
        let op2Src = sourceRegionToDiagnosePosition $ op2 ^. sourceRegion
        writeReport $
            Err
                (Just Codes.samePrecedence)
                ("Cannot mix operators with same precedence " <> fullNameText (op1 ^. unlocated) <> " and " <> fullNameText (op2 ^. unlocated) <> " when both operators have different associativity.")
                [(op1Src, This (show a1)), (op2Src, This (show a2))]
                [Hint "Add parentheses to resolve the ambiguity", Hint "Change the precedence of one of the operators", Hint "Change the associativity of one of the operators"]

newtype ShuntWarning
    = UnknownPrecedence (Located (Qualified Name))
    deriving (Show, Eq, Ord)

instance ReportableError ShuntWarning where
    report (UnknownPrecedence op) = do
        let opSrc = sourceRegionToDiagnosePosition $ op ^. sourceRegion
        writeReport $
            Warn
                (Just Codes.unknownPrecedence)
                ("Unknown precedence/associativity for operator " <> fullNameText (op ^. unlocated) <> ". The system will assume it has the highest precedence (9) and left associativity, but you should specify it manually. ")
                [(opSrc, This "operator")]
                [Hint "Define the precedence and associativity of the operator explicitly"]

opInfo :: OpTable -> Annotated.BinaryOperator -> Maybe OpInfo
opInfo table op = case op ^. Annotated._MkBinaryOperator . unlocated of
    Annotated.Op opName -> lookup (NOpName <$> opName ^. unlocated) table
    Annotated.Infixed varName -> lookup (NVarName <$> varName ^. unlocated) table

pattern InExpr :: Annotated.Expr' -> Annotated.Expr
pattern InExpr y <- Annotated.Expr (Located _ y)

pattern InExpr' :: SourceRegion -> Annotated.Expr' -> Annotated.Expr
pattern InExpr' loc y <- Annotated.Expr (Located loc y)

{-
 | Fix the operators in an expression to the correct precedence
 | For example given ((+) = 1l) and ((*) = 2r)
 | 1 + 2 * 3 * 4 + 5 + 6 should be parsed as (((1 + (2 * 3)) * 4) + 5) + 6
 | https://stackoverflow.com/a/67992584/6272977 This answer was a huge help in designing this
-}
fixOperators :: forall r. (Member (Error ShuntError) r, Member (Writer (Set ShuntWarning)) r) => OpTable -> Annotated.Expr -> Sem r Annotated.Expr
fixOperators opTable = reassoc
  where
    withLocationOf :: Annotated.Expr -> Annotated.Expr' -> Annotated.Expr
    withLocationOf s repl = over Annotated._Expr (repl <$) s

    reassoc :: Annotated.Expr -> Sem r Annotated.Expr
    reassoc e@(InExpr (Annotated.InParens e2)) = withLocationOf e . Annotated.InParens <$> reassoc e2
    reassoc e@(InExpr' loc (Annotated.BinaryOperator op l r)) = do
        l' <- reassoc l
        r' <- reassoc r
        withLocationOf e <$> reassoc' loc op l' r'
    reassoc e = pure e

    reassoc' :: SourceRegion -> Annotated.BinaryOperator -> Annotated.Expr -> Annotated.Expr -> Sem r Annotated.Expr'
    reassoc' sr op l (InExpr (Annotated.InParens r)) = reassoc' sr op l r
    reassoc' sr o1 e1 r@(InExpr (Annotated.BinaryOperator o2 e2 e3)) = do
        info1 <- getInfoOrWarn o1
        info2 <- getInfoOrWarn o2
        case compare info1.precedence info2.precedence of
            GT -> assocLeft
            LT -> assocRight
            EQ -> case (info1.associativity, info2.associativity) of
                (LeftAssociative, LeftAssociative) -> assocLeft
                (RightAssociative, RightAssociative) -> assocRight
                (a1, a2) -> throw (SamePrecedenceError (Annotated.locatedOperatorName o1, a1) (Annotated.locatedOperatorName o2, a2))
      where
        assocLeft = do
            reassociated <- Annotated.Expr . Located sr <$> reassoc' sr o1 e1 e2
            pure (Annotated.BinaryOperator o2 (withLocationOf reassociated (Annotated.InParens reassociated)) e3)

        assocRight = do
            pure (Annotated.BinaryOperator o1 e1 r)

        getInfoOrWarn :: Annotated.BinaryOperator -> Sem r OpInfo
        getInfoOrWarn op = case opInfo opTable op of
            Just info -> pure info
            Nothing -> do
                tell (fromList [UnknownPrecedence (Annotated.locatedOperatorName op)])
                pure (OpInfo (mkPrecedence 9) LeftAssociative)
    reassoc' _ op l r = pure (Annotated.BinaryOperator op l r)