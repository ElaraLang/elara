module Canonicalize.Pattern where

import AST.Canonical qualified as Can
import AST.Source qualified as Src

canonicalize :: Src.Pattern -> Can.Pattern
canonicalize pat = do
  case pat of
    Src.PWildcard -> Can.PWildcard
    Src.PVar name -> Can.PVar name
