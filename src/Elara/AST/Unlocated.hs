module Elara.AST.Unlocated where

import Data.Kind qualified as Kind
import Elara.AST.Generic
import Elara.AST.Select (LocatedAST, UnlocatedAST)

type family Replace (needle :: LocatedAST) (replacement :: UnlocatedAST) (haystack :: Kind.Type) where
    Replace needle replacement (Expr needle) = Expr replacement
    Replace needle replacement (Expr' needle) = Expr' replacement
    Replace needle replacement (Pattern needle) = Pattern replacement
    Replace needle replacement (Pattern' needle) = Pattern' replacement
    Replace needle replacement (BinaryOperator needle) = BinaryOperator replacement
    Replace needle replacement (BinaryOperator' needle) = BinaryOperator' replacement
    Replace needle replacement (Type needle) = Type replacement
    Replace needle replacement (Type' needle) = Type' replacement
    Replace needle replacement (Declaration needle) = Declaration replacement
    Replace needle replacement (Declaration' needle) = Declaration' replacement
    Replace needle replacement (DeclarationBody needle) = DeclarationBody replacement
    Replace needle replacement (DeclarationBody' needle) = DeclarationBody' replacement
    Replace needle replacement [list] = [Replace needle replacement list]
    -- Replace needle replacement (Select f needle) = Select f replacement
    Replace needle replacement other = other
