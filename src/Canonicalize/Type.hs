module Canonicalize.Type where

import AST.Canonical qualified as Can
import AST.Source qualified as Src

canonicalize :: Src.Type -> Can.Type
canonicalize type' = do
  case type' of
    Src.TVar x -> Can.TVar x
    Src.TUnit -> Can.TUnit
    Src.TLambda a b -> Can.TLambda (canonicalize a) (canonicalize b)
    other -> error $ "Canonicalize.Type.canonicalize: " ++ show other