module Elara.Name where

import Elara.String as ES

data Name
  = VarName ES.String
  | OpName ES.String
  | TypeName ES.String
  | QualifiedName Name Name

instance Show Name where
  show (VarName s) = show s
  show (OpName s) = "(" ++ show s ++ ")"
  show (TypeName s) = show s
  show (QualifiedName n1 n2) = show n1 ++ "." ++ show n2