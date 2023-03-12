{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Frontend where

import Control.Lens.TH
import Elara.AST.Name (MaybeQualified, Name, OpName, TypeName, VarName, Unqualified)
import Elara.AST.Region (Located)
import Prelude hiding (Type)

-- | Frontend AST
data Expr'
  = Int Integer
  | Float Double
  | String Text
  | Char Char
  | Unit
  | Var (Located (MaybeQualified VarName))
  | Constructor (Located (MaybeQualified TypeName))
  | Lambda [Pattern] Expr
  | FunctionCall Expr Expr
  | If Expr Expr Expr
  | BinaryOperator BinaryOperator Expr Expr
  | List [Expr]
  | LetIn (Located (Unqualified VarName)) [Pattern] Expr Expr
  | Let (Located (Unqualified VarName)) [Pattern] Expr
  | Block (NonEmpty Expr)
  | InParens Expr
  deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
  deriving (Show, Eq)

data Pattern'
  = NamedPattern Text
  | ConstructorPattern (Located (MaybeQualified TypeName)) [Pattern]
  | ListPattern [Pattern]
  | WildcardPattern
  deriving (Show, Eq)

newtype Pattern = Pattern (Located Pattern')
  deriving (Show, Eq)

data BinaryOperator'
  = Op (Located (MaybeQualified OpName))
  | Infixed (Located (MaybeQualified VarName))
  deriving (Show, Eq)

newtype BinaryOperator = MkBinaryOperator (Located BinaryOperator')
  deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation (Located (MaybeQualified Name)) Type
  deriving (Show, Eq)

data Type
  = TypeVar Text
  | FunctionType Type Type
  | UnitType
  | TypeConstructorApplication Type Type
  | UserDefinedType (Located (MaybeQualified TypeName))
  deriving (Show, Eq)


makePrisms ''Expr
makePrisms ''Pattern
