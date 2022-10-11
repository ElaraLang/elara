{-# LANGUAGE DataKinds #-}

module Elara.AST.Canonical where

import Control.Lens
import Data.Data (Data)
import Data.Map qualified as M
import Elara.AST.Generic (PatternLike (patternNames))
import Elara.Data.Located
import Elara.Data.Module (Module)
import Elara.Data.Name (ModuleName, Name (..), QualifiedName)
import Elara.Data.Qualifications (Qualified)
import Elara.Data.Type (ConcreteType)
import Elara.Data.Uniqueness
import Prelude hiding (Type)

newtype ProjectFields = ProjectFields
  { modules :: M.Map ModuleName (Module LocatedExpr Pattern (ConcreteType Qualified) Qualified 'Unique)
  }

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
  | Argument Name
  | Var QualifiedName
  | Constructor QualifiedName
  | Lambda {arg :: Pattern, body :: LocatedExpr}
  | FunctionCall {function :: LocatedExpr, argument :: LocatedExpr}
  | BinaryOperator {operator :: LocatedExpr, left :: LocatedExpr, right :: LocatedExpr}
  | If {condition :: LocatedExpr, then_ :: LocatedExpr, else_ :: LocatedExpr}
  | Block (NonEmpty LocatedExpr)
  | List [LocatedExpr]
  | Unit
  | LetIn {name :: Name, value :: LocatedExpr, body :: LocatedExpr}
  deriving (Show, Eq, Data)

data Pattern
  = NamedPattern Name
  | WildPattern
  deriving (Eq, Ord, Show, Data)

instance PatternLike Pattern where
  patternNames (NamedPattern n) = [n]
  patternNames WildPattern = []

data Type
  = TypeVar Name
  | Function {_from :: Type, _to :: Type}
  | UnitT
  | TypeConstructorApplication {_constructor :: Type, _args :: [Type]}
  | UserDefinedType
      { _qualified :: Qualified
      , _name :: Name
      }