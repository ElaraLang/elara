-- | A teeny tiny interpreter for evaluating constant expressions, eg annotation values
module Elara.ConstExpr where

import Effectful
import Elara.AST.Generic (AnnotationArg (..), Expr (..), Expr' (..), RUnlocate (..))
import Elara.AST.Region

data ConstVal
    = ConstInt Integer
    | ConstString Text
    | ConstChar Char
    | ConstUnit
    deriving (Show, Eq)

interpretAnnotationArg :: RUnlocate ast => AnnotationArg ast -> Eff r ConstVal
interpretAnnotationArg (AnnotationArg e) = interpretAnnotationArg' e

interpretAnnotationArg' :: forall ast r. RUnlocate ast => Expr ast -> Eff r ConstVal
interpretAnnotationArg' (Expr (le, _)) = interpretAnnotationArg'' (le ^. rUnlocated @_ @ast @(Expr' ast))

interpretAnnotationArg'' :: Expr' ast -> Eff r ConstVal
interpretAnnotationArg'' (Int n) = pure $ ConstInt n
interpretAnnotationArg'' (String s) = pure $ ConstString s
interpretAnnotationArg'' (Char c) = pure $ ConstChar c
interpretAnnotationArg'' Unit = pure ConstUnit
interpretAnnotationArg'' other = error "Non-constant expression in annotation: " -- TODO
