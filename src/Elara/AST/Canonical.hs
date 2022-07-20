module Elara.AST.Canonical where

import Elara.Data.Located
import Elara.Data.Name (Name, QualifiedName)

type LocatedExpr = Located Expr

{-
Similar to the Frontend AST but with a few simple changes to make later processing easier:
- All names are fully qualified
- All lambdas only have a single argument

-}
data Expr
  = Int Int
  | Float Double
  | Char Char
  | String String
  | Bool Bool
  | Var QualifiedName
  | Constructor QualifiedName
  | Lambda {arg :: QualifiedName, body :: LocatedExpr}
  | FunctionCall {function :: LocatedExpr, argument :: LocatedExpr}
  | BinaryOperator {operator :: LocatedExpr, left :: LocatedExpr, right :: LocatedExpr}
  | If {condition :: LocatedExpr, then_ :: LocatedExpr, else_ :: LocatedExpr}
  | Block [LocatedExpr]
  | Unit
  deriving (Show)

data Pattern
  = NamedPattern QualifiedName
  | WildPattern
  deriving (Eq, Ord, Show)