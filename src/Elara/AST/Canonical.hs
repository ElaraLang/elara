module Elara.AST.Canonical where

import Data.Map qualified as M
import Elara.AST.Generic (PatternLike (patternNames))
import Elara.Data.Located
import Elara.Data.Module (Module)
import Elara.Data.Name (ModuleName, Name (..), QualifiedName)
import Elara.Data.Qualifications (Qualified)
import Elara.Data.Type (ConcreteType)

newtype ProjectFields = ProjectFields
  { modules :: M.Map ModuleName (Module LocatedExpr Pattern (ConcreteType Qualified) Qualified)
  }

type LocatedExpr = (Located Expr)

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
  | Argument Name
  | Var QualifiedName
  | Constructor QualifiedName
  | Lambda {arg :: Pattern, body :: LocatedExpr}
  | FunctionCall {function :: LocatedExpr, argument :: LocatedExpr}
  | BinaryOperator {operator :: LocatedExpr, left :: LocatedExpr, right :: LocatedExpr}
  | If {condition :: LocatedExpr, then_ :: LocatedExpr, else_ :: LocatedExpr}
  | Block [LocatedExpr]
  | Unit
  deriving (Show)

data Pattern
  = NamedPattern Name
  | WildPattern
  deriving (Eq, Ord, Show)

instance PatternLike Pattern where
  patternNames (NamedPattern name) = [name]
  patternNames WildPattern = []