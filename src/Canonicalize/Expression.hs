module Canonicalize.Expression where

import AST.Canonical qualified as Can
import AST.Source qualified as Src
import Canonicalize.Pattern qualified as Pat

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
    Src.Lambda a b -> Can.Lambda (Pat.canonicalize <$> a) (canonicalize b)
    Src.Let def e -> Can.Let (canonicalizeDef def) (canonicalize e)
    other -> error $ "Can't canonicalize " ++ show other

canonicalizeDef :: Src.Def -> Can.Def
canonicalizeDef (Src.Define name pats expr) = Can.Def name (Pat.canonicalize <$> pats) (canonicalize expr)
canonicalizeDef other = error $ "Can't canonicalize " ++ show other