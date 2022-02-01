module AST.Source (Expr, Expr (..), Identifier, Identifier (..)) where

data Identifier
  = NormalIdentifier String
  | OpIdentifier String
  deriving (Show, Eq)

data Expr
  = StringE String
  | IntE Integer
  | FloatE Double
  | CharE Char
  | VarE Identifier Expr
  deriving (Show, Eq)
