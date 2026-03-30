module Elara.ConstExpr where

import Effectful
import Elara.AST.Types qualified as New

data ConstVal
    = ConstInt Integer
    | ConstString Text
    | ConstChar Char
    | ConstUnit
    deriving (Show, Eq)

interpretNewAnnotationArg :: New.AnnotationArg loc p -> Eff r ConstVal
interpretNewAnnotationArg (New.AnnotationArg (New.Expr _ _ e')) = interpretNewExpr e'

interpretNewExpr :: New.Expr' loc p -> Eff r ConstVal
interpretNewExpr (New.EInt n) = pure $ ConstInt n
interpretNewExpr (New.EString s) = pure $ ConstString s
interpretNewExpr (New.EChar c) = pure $ ConstChar c
interpretNewExpr New.EUnit = pure ConstUnit
interpretNewExpr _ = error "Non-constant expression in annotation: " -- TODO
