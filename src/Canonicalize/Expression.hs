module Canonicalize.Expression where

import AST.Canonical qualified as Can
import AST.Source qualified as Src

canonicalize :: Src.Expr -> Can.Expr
canonicalize expr = do
  case expr of
    Src.Char c -> Can.Char c
    Src.String c -> Can.String c
    Src.Float c -> Can.Float c
    Src.Int c -> Can.Int c
    Src.Var name -> Can.Var name
    Src.BinOp op a b -> Can.BinOp (canonicalize op) (canonicalize a) (canonicalize b)
    Src.BlockExpr [single] -> canonicalize single
    Src.BlockExpr many -> Can.BlockExpr (canonicalize <$> many)
    Src.List exprs -> Can.List (canonicalize <$> exprs)
    Src.FunctionCall a b -> Can.FunctionCall (canonicalize a) (canonicalize b)
    other -> error $ "Can't canonicalize " ++ show other