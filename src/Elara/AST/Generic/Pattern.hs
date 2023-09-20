{-# LANGUAGE PatternSynonyms #-}

module Elara.AST.Generic.Pattern where

import Elara.AST.Generic

pattern FunctionCall' a b <- Expr (FunctionCall a b, _)

functionCall ::
    forall {a1} {a2} {ast1 :: a1} {ast2 :: a2}.
    _ =>
    Expr ast2 ->
    Expr ast2 ->
    Expr ast1
functionCall a b = Expr (FunctionCall a b, Nothing)

var a = Expr (Var a, Nothing)

int ::
    forall {a1} {a2} {ast1 :: a1} {ast2 :: a2} {a3}.
    ( CleanupLocated (ASTLocate' ast1 (Expr' ast1)) ~ Expr' ast2
    , Select "ExprType" ast1 ~ Maybe a3
    ) =>
    Integer ->
    Expr ast1
int a = Expr (Int a, Nothing)
