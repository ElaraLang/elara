{-# LANGUAGE AllowAmbiguousTypes #-}

module Elara.ConstExpr where

import Elara.AST.Extensions (InParensExtension (..), ListExprExtension (..), TupleExprExtension (..))
import Elara.AST.Phase (ExpressionExtension)
import Elara.AST.Phases.Desugared (Desugared, DesugaredExpressionExtension (..))
import Elara.AST.Phases.Frontend (Frontend, FrontendExpressionExtension (..))
import Elara.AST.Phases.Renamed (Renamed, RenamedExpressionExtension (..))
import Elara.AST.Types qualified as New
import Elara.Data.AtLeast2List (AtLeast2List)

data ConstVal
    = ConstInt Integer
    | ConstString Text
    | ConstChar Char
    | ConstUnit
    | ConstList [ConstVal]
    | ConstTuple (AtLeast2List ConstVal)
    deriving (Show, Eq)

class InterpretConstExpr p where
    interpretExprExtension :: ExpressionExtension p loc -> Maybe ConstVal

interpretExpr :: forall p loc. InterpretConstExpr p => New.Expr loc p -> Maybe ConstVal
interpretExpr (New.Expr _ _ e') = interpretExpr' e'

interpretExpr' :: forall p loc. InterpretConstExpr p => New.Expr' loc p -> Maybe ConstVal
interpretExpr' (New.EInt n) = Just $ ConstInt n
interpretExpr' (New.EString s) = Just $ ConstString s
interpretExpr' (New.EChar c) = Just $ ConstChar c
interpretExpr' New.EUnit = Just ConstUnit
interpretExpr' (New.EExtension ext) = interpretExprExtension @p @loc ext
interpretExpr' _ = Nothing

interpretNewAnnotationArg :: forall p loc. InterpretConstExpr p => New.AnnotationArg loc p -> Maybe ConstVal
interpretNewAnnotationArg (New.AnnotationArg e) = interpretExpr e

instance InterpretConstExpr Frontend where
    interpretExprExtension = \case
        FrontendInParens (InParensExpression inner) -> interpretExpr inner
        FrontendList (ListExpression elems) -> ConstList <$> traverse interpretExpr elems
        FrontendTuple (TupleExpression elems) -> ConstTuple <$> traverse interpretExpr elems
        _ -> Nothing

instance InterpretConstExpr Desugared where
    interpretExprExtension = \case
        DesugaredInParens (InParensExpression inner) -> interpretExpr inner
        DesugaredList (ListExpression elems) -> ConstList <$> traverse interpretExpr elems
        DesugaredTuple (TupleExpression elems) -> ConstTuple <$> traverse interpretExpr elems
        _ -> Nothing

instance InterpretConstExpr Renamed where
    interpretExprExtension = \case
        RenamedInParens (InParensExpression inner) -> interpretExpr inner
        RenamedBinaryOperator{} -> Nothing
