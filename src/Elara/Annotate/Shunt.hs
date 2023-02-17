{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elara.Annotate.Shunt (Precedence, mkPrecedence, OpTable, OpInfo (..), Associativity (..), ShuntError (..), ShuntWarning (..), fixOperators) where

import Control.Lens (over, view)
import Data.Map (lookup)
import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Name (Name (NOpName, NVarName), Qualified)
import Elara.AST.Region (Located (..), SourceRegion, unlocate)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.Writer
import Prelude hiding (State, execState, gets, modify')

type OpTable = Map (Name Qualified) OpInfo

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
    = SamePrecedenceError (Name Qualified) (Name Qualified)
    deriving (Show, Eq)

newtype ShuntWarning
    = UnknownPrecedence (Name Qualified)
    deriving (Show, Eq, Ord)

opInfo :: OpTable -> Annotated.BinaryOperator -> Maybe OpInfo
opInfo table op = case unlocate $ view Annotated._MkBinaryOperator op of
    Annotated.Op opName -> lookup (NOpName opName) table
    Annotated.Infixed varName -> lookup (NVarName varName) table


pattern InExpr :: Annotated.Expr' -> Annotated.Expr
pattern InExpr y <- Annotated.Expr (Located _ y)

pattern InExpr' :: SourceRegion -> Annotated.Expr' -> Annotated.Expr
pattern InExpr' loc y <- Annotated.Expr (Located loc y)

{- | Fix the operators in an expression to the correct precedence
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
                _ -> throw (SamePrecedenceError (Annotated.operatorName o1) (Annotated.operatorName o2))
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
                tell (fromList [UnknownPrecedence (Annotated.operatorName op)])
                pure (OpInfo (mkPrecedence 9) LeftAssociative)
    reassoc' _ op l r = pure (Annotated.BinaryOperator op l r)