module Elara.AST.Frontend where

import Data.Map qualified as M
import Elara.AST.Generic (PatternLike (patternNames))
import Elara.Data.Located (Located)
import Elara.Data.Module (Module)
import Elara.Data.Name (ModuleName, Name)
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.TypeAnnotation (TypeAnnotation)

{- Least abstract AST, closest to elara source code.
Things like comments are preserved
 -}

-- Information about the whole project
newtype ProjectFields = ProjectFields
  { modules :: M.Map ModuleName (Module LocatedExpr Pattern TypeAnnotation MaybeQualified)
  }

type LocatedExpr = Located Expr

data Expr
  = Int Int
  | Float Double
  | Char Char
  | String String
  | Bool Bool -- Do we want Bool has a primitive or a constructor?
  | Unit
  | Var Name
  | Constructor Name
  | Lambda {arguments :: [Name], body :: LocatedExpr}
  | FunctionCall {function :: LocatedExpr, argument :: LocatedExpr}
  | BinaryOperator {operator :: LocatedExpr, left :: LocatedExpr, right :: LocatedExpr}
  | If {condition :: LocatedExpr, then_ :: LocatedExpr, else_ :: LocatedExpr}
  | Block [LocatedExpr]
  deriving (Show, Eq)

data Pattern
  = NamedPattern Name
  | WildPattern
  deriving (Eq, Ord, Show)

instance PatternLike Pattern where
  patternNames (NamedPattern name) = [name]
  patternNames WildPattern = []