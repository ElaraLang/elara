{-# LANGUAGE DataKinds #-}

module Elara.AST.Typed where

import Data.Data (Data)
import Data.Map qualified as M
import Elara.AST.Generic (PatternLike (patternNames))
import Elara.Data.Located
import Elara.Data.Module (Declaration, DeclarationBody, Module)
import Elara.Data.Name (
  ModuleName,
  Name (..),
  QualifiedName,
 )
import Elara.Data.Qualifications (Qualified)
import Elara.Data.Type (ConcreteType)
import Elara.Data.Uniqueness
import Elara.TypeInfer.Common (Scheme, Type)
import Prelude hiding (Type)

newtype ProjectFields = ProjectFields
  { modules :: M.Map ModuleName TypedModule
  }

type TypedModule = Module PolytypeExpr Void (ConcreteType Qualified) Qualified 'Unique
type TypedDeclaration = Declaration PolytypeExpr Void (ConcreteType Qualified) Qualified
type TypedDeclarationBody = DeclarationBody PolytypeExpr Void (ConcreteType Qualified) Qualified

type LocatedExpr = Located Expr

data PolytypeExpr = PolytypeExpr
  { polytypeExpr :: LocatedExpr
  , polytype :: Scheme
  }
  deriving (Eq, Show, Data)

data Expr = Expr Expr_ Type
  deriving (Show, Eq, Data)

typeOf :: Expr -> Type
typeOf (Expr _ t) = t

{-
\| Similar to the Canonical AST but every element now has a static type
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
  | Lambda Pattern LocatedExpr
  | FunctionCall LocatedExpr LocatedExpr
  | BinaryOperator LocatedExpr LocatedExpr LocatedExpr
  | -- | Operator, left, right
    If LocatedExpr LocatedExpr LocatedExpr -- | condition, then, else
  | Block (NonEmpty LocatedExpr)
  | List [LocatedExpr]
  | Unit
  | LetIn Name LocatedExpr LocatedExpr Scheme
  | -- | Name of the binding, value, body, and the scheme of the value
    Fix LocatedExpr -- Fix point, used for type inference
  deriving (Show, Eq, Data)

transformExpr :: (Expr -> Expr) -> Expr -> Expr
transformExpr f = \case
  Expr e t -> f (Expr (transformExpr_ f e) t)

transformExpr_ :: (Expr -> Expr) -> Expr_ -> Expr_
transformExpr_ f (Lambda arg body) = Lambda arg (transformExpr f <$> body)
transformExpr_ f (FunctionCall function argument) =
  FunctionCall (transformExpr f <$> function) (transformExpr f <$> argument)
transformExpr_ f (BinaryOperator operator left right) =
  BinaryOperator
    (transformExpr f <$> operator)
    (transformExpr f <$> left)
    (transformExpr f <$> right)
transformExpr_ f (If condition then_ else_) =
  If
    (transformExpr f <$> condition)
    (transformExpr f <$> then_)
    (transformExpr f <$> else_)
transformExpr_ f (Block exprs) = Block ((transformExpr f <$>) <$> exprs)
transformExpr_ f (List exprs) = List ((transformExpr f <$>) <$> exprs)
transformExpr_ f (LetIn name value body scheme) =
  LetIn name (transformExpr f <$> value) (transformExpr f <$> body) scheme
transformExpr_ f (Fix expr) = Fix (transformExpr f <$> expr)
transformExpr_ _ e = e

data Pattern = Pattern Pattern_ (Maybe Type)
  deriving (Show, Eq, Data)

data Pattern_
  = NamedPattern Name
  | WildPattern
  deriving (Eq, Ord, Show, Data)

instance PatternLike Pattern_ where
  patternNames (NamedPattern n) = [n]
  patternNames WildPattern = []

instance PatternLike Pattern where
  patternNames (Pattern p _) = patternNames p