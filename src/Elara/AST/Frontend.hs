{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Frontend where

import Control.Lens hiding (element, op)
import Data.Data (Data, Typeable)
import Data.Map qualified as M
import Elara.AST.Generic (PatternLike (patternNames))
import Elara.Data.Located (IsLocated, NoLocated, XRec)
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

type LocatedExpr = RExpr IsLocated

type UnwrappedExpr = RExpr NoLocated

type RExpr p = XRec p (Expr p)

data Expr x
  = Int Int
  | Float Double
  | Char Char
  | String String
  | Bool Bool -- Do we want Bool has a primitive or a constructor?
  | Unit
  | Var Name
  | Constructor Name
  | Lambda {arguments :: [Pattern], body :: RExpr x}
  | -- | Refers to a locally scoped variable defined as a lambda parameter or from a let binding, makes canonicalization easier
    Argument Name
  | FunctionCall {function :: RExpr x, argument :: RExpr x}
  | BinaryOperator {operator :: RExpr x, left :: RExpr x, right :: RExpr x}
  | If {condition :: RExpr x, then_ :: RExpr x, else_ :: RExpr x}
  | Block [RExpr x]

deriving instance (Show (RExpr x)) => Show (Expr x)

deriving instance (Eq (RExpr x)) => Eq (Expr x)

deriving instance (Typeable x, Data x, Data (RExpr x)) => Data (Expr x)

instance Traversable (XRec x) => Plated (Expr x) where
  plate f expr = case expr of
    Lambda args body -> Lambda args <$> traverse f body
    FunctionCall fun arg -> FunctionCall <$> traverse f fun <*> traverse f arg
    BinaryOperator op left right -> BinaryOperator op <$> traverse f left <*> traverse f right
    If cond then_ else_ -> If <$> traverse f cond <*> traverse f then_ <*> traverse f else_
    Block exprs -> Block <$> traverse (traverse f) exprs
    other -> pure other

data Pattern
  = NamedPattern Name
  | WildPattern
  deriving (Eq, Ord, Show, Data)

makeLenses ''Expr
makeLenses ''Pattern

instance PatternLike Pattern where
  patternNames (NamedPattern name) = [name]
  patternNames WildPattern = []