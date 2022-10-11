{-# LANGUAGE DataKinds #-}

module Elara.AST.Typed where

import qualified Data.Map                      as M
import           Elara.AST.Generic              ( PatternLike(patternNames) )
import           Elara.Data.Located
import           Elara.Data.Module              ( Module, Declaration )
import           Elara.Data.Name                ( ModuleName
                                                , Name(..)
                                                , QualifiedName
                                                )
import           Elara.Data.Qualifications      ( Qualified )
import           Elara.Data.Type                ( ConcreteType )
import           Elara.Data.Uniqueness
import           Prelude                 hiding ( Type )

newtype ProjectFields = ProjectFields
  { modules :: M.Map ModuleName (Module LocatedExpr Void (ConcreteType Qualified) Qualified 'Unique)
  }


type TypedDeclaration = Declaration LocatedExpr Void (ConcreteType Qualified) Qualified

type LocatedExpr = Located Expr

data Expr = Expr Expr_ Type
  deriving (Show, Eq)

typeOf :: Expr -> Type
typeOf (Expr _ t) = t

{-
Similar to the Canonical AST but every element now has a static type
-}
data Expr_
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
  deriving (Show, Eq)

data Pattern = Pattern Pattern_ (Maybe Type)
  deriving (Show, Eq)

data Pattern_
  = NamedPattern Name
  | WildPattern
  deriving (Eq, Ord, Show)

instance PatternLike Pattern_ where
  patternNames (NamedPattern n) = [n]
  patternNames WildPattern      = []

instance PatternLike Pattern where
  patternNames (Pattern p _) = patternNames p

data Type
  = TypeVar Text
  | Type :-> Type
  | TypeConstructorApplication {_constructor :: Type, _args :: [Type]}
  | UserDefinedType
      { _qualified :: Qualified,
        _name :: Name
      }
  deriving (Eq, Show, Ord)
