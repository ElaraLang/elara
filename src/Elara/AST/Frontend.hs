{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Frontend where

import Control.Lens hiding (List, element, op)
import Data.Data (Data)
import Data.Map qualified as M
import Elara.AST.Generic (PatternLike (patternNames))
import Elara.Data.Located (IsLocated, Located (Located), NoLocated, XRec, TypeIdentity (TypeIdentity))
import Elara.Data.Module (Module)
import Elara.Data.Name (ModuleName, Name)
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.TypeAnnotation (TypeAnnotation)
import Elara.Data.Uniqueness
import Prelude hiding (Type)

{- Least abstract AST, closest to elara source code.
Things like comments are preserved
 -}

-- Information about the whole project
newtype ProjectFields = ProjectFields
  { modules :: M.Map ModuleName (Module LocatedExpr Pattern TypeAnnotation MaybeQualified 'Many)
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
  | List [RExpr x]
  | Let {name :: Name, arguments :: [Pattern], body :: RExpr x}
  | LetIn {name :: Name, arguments :: [Pattern], value :: RExpr x, body :: RExpr x}

mapXRec :: (Functor (XRec b)) => (forall x. XRec a x -> XRec b x) -> Expr a -> Expr b
mapXRec _ (Int i) = Int i
mapXRec _ (Float d) = Float d
mapXRec _ (Char c) = Char c
mapXRec _ (String s) = String s
mapXRec _ (Bool b) = Bool b
mapXRec _ Unit = Unit
mapXRec _ (Var n) = Var n
mapXRec _ (Constructor n) = Constructor n
mapXRec f (Lambda args body) = Lambda args (mapRExpr f body)
mapXRec _ (Argument n) = Argument n
mapXRec f (FunctionCall f' arg) = FunctionCall (mapRExpr f f') (mapRExpr f arg)
mapXRec f (BinaryOperator op l r) = BinaryOperator (mapRExpr f op) (mapRExpr f l) (mapRExpr f r)
mapXRec f (If c t e) = If (mapRExpr f c) (mapRExpr f t) (mapRExpr f e)
mapXRec f (Block b) = Block (fmap (mapRExpr f) b)
mapXRec f (List l) = List (fmap (mapRExpr f) l)
mapXRec f (Let n args body) = Let n args (mapRExpr f body)
mapXRec f (LetIn n args v body) = LetIn n args (mapRExpr f v) (mapRExpr f body)

mapRExpr :: (Functor (XRec b)) => (forall x. XRec a x -> XRec b x) -> RExpr a -> RExpr b
mapRExpr f x = fmap (mapXRec f) (f x)

unlocateExpr :: LocatedExpr -> UnwrappedExpr
unlocateExpr = mapRExpr unlocateExpr'
  where
    unlocateExpr' (Located _ e) = TypeIdentity e

deriving instance (Show (RExpr x)) => Show (Expr x)

deriving instance (Eq (RExpr x)) => Eq (Expr x)

deriving instance (Ord (RExpr x)) => Ord (Expr x)

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

data Type
  = TypeVar Name
  | Function {_from :: Type, _to :: Type}
  | UnitT
  | TypeConstructorApplication {_constructor :: Type, _arg :: Type}
  | UserDefinedType
      { _qualified :: MaybeQualified,
        _name :: Name
      }