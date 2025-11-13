{-# LANGUAGE PatternSynonyms #-}

module Elara.AST.Generic.Pattern where

import Elara.AST.Generic
import Elara.AST.Select

pattern FunctionCall' :: ASTLocate ast1 (Expr' ast1) ~ Expr' ast2 => Expr ast2 -> Expr ast2 -> Expr ast1
pattern FunctionCall' a b <- Expr (FunctionCall a b, _)

functionCall ::
    forall a {a1} {a2} {ast1 :: a1} {ast2 :: a2}.
    (ASTLocate ast1 (Expr' ast1) ~ Expr' ast2, Select (ASTType ForExpr) ast1 ~ Maybe a) =>
    Expr ast2 ->
    Expr ast2 ->
    Expr ast1
functionCall a b = Expr (FunctionCall a b, Nothing)

var ::
    forall {a1} {a2} {ast1 :: a1} {ast2 :: a2} {a3}.
    ( ASTLocate ast1 (Expr' ast1) ~ Expr' ast2
    , Select (ASTType ForExpr) ast1 ~ Maybe a3
    ) =>
    ASTLocate ast2 (Select ASTVarRef ast2) ->
    Expr ast1
var a = Expr (Var a, Nothing)

int ::
    forall {a1} {a2} {ast1 :: a1} {ast2 :: a2} {a3}.
    ( CleanupLocated (ASTLocate' ast1 (Expr' ast1)) ~ Expr' ast2
    , Select (ASTType ForExpr) ast1 ~ Maybe a3
    ) =>
    Integer ->
    Expr ast1
int a = Expr (Int a, Nothing)
