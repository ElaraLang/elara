module Elara.AST.Frontend where

import Elara.AST.Name (MaybeQualified, Name, OpName, TypeName, VarName)
import Elara.AST.Region (Located)
import Elara.Data.Type (Type (..))
import Prelude hiding (Type)

-- | Frontend AST
data Expr'
  = Int Integer
  | Float Double
  | String Text
  | Char Char
  | Unit
  | Var (MaybeQualified VarName)
  | Constructor (MaybeQualified TypeName)
  | Lambda [Pattern] Expr
  | FunctionCall Expr Expr
  | If Expr Expr Expr
  | BinaryOperator BinaryOperator Expr Expr
  | List [Expr]
  | LetIn (MaybeQualified VarName) [Pattern] Expr Expr
  | Let (MaybeQualified VarName) [Pattern] Expr
  | Block (NonEmpty Expr)
  | InParens Expr
  deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
  deriving (Show, Eq)

data Pattern'
  = NamedPattern Text
  | ConstructorPattern (MaybeQualified TypeName) [Pattern]
  | ListPattern [Pattern]
  | WildcardPattern
  deriving (Show, Eq)

newtype Pattern = Pattern (Located Pattern')
  deriving (Show, Eq)

data BinaryOperator'
  = Op (MaybeQualified OpName)
  | Infixed (MaybeQualified VarName)
  deriving (Show, Eq)

newtype BinaryOperator = MkBinaryOperator (Located BinaryOperator')
  deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation (Name MaybeQualified) (Type MaybeQualified)
  deriving (Show, Eq)