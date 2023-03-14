{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Annotated where

import Control.Lens (makePrisms)
import Elara.AST.Name (Name (NOpName, NVarName), OpName, Qualified, TypeName, Unqualified, VarName)
import Elara.AST.Region (Located (Located))
import Prelude hiding (Op, Type)

{- |
  This is the second main AST stage, which is very similar to the `Elara.AST.Frontend.Expr` AST, with a few key differences:

    * Everything is explicitly qualified with its module name (if applicable)
    * Lambdas only have 1 argument (ones with multiple arguments are desugared into nested lambdas)
    * Let bindings have no patterns, they are desugared into lambdas
-}
data Expr'
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (Located (Qualified VarName))
    | Constructor (Located (Qualified TypeName))
    | Lambda Pattern Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | BinaryOperator BinaryOperator Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn (Located (Unqualified VarName)) Expr Expr
    | Let (Located (Unqualified VarName)) Expr
    | Block (NonEmpty Expr)
    | InParens Expr
    deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
    deriving (Show, Eq)

data Pattern'
    = NamedPattern Text
    | ConstructorPattern (Located (Qualified TypeName)) [Pattern]
    | ListPattern [Pattern]
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq)

newtype Pattern = Pattern (Located Pattern')
    deriving (Show, Eq)

data BinaryOperator'
    = Op (Located (Qualified OpName))
    | Infixed (Located (Qualified VarName))
    deriving (Show, Eq)

locatedOperatorName :: BinaryOperator -> Located (Qualified Name)
locatedOperatorName (MkBinaryOperator (Located _ (Op op))) = NOpName <<$>> op
locatedOperatorName (MkBinaryOperator (Located _ (Infixed op))) = NVarName <<$>> op

newtype BinaryOperator = MkBinaryOperator (Located BinaryOperator')
    deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation (Located (Qualified Name)) Type
    deriving (Show, Eq)

data Type
    = TypeVar Text
    | FunctionType Type Type
    | UnitType
    | TypeConstructorApplication Type Type
    | UserDefinedType (Located (Qualified TypeName))
    | RecordType (NonEmpty (Located (Unqualified VarName), Type))
    deriving (Show, Eq)

makePrisms ''Expr
makePrisms ''Pattern
makePrisms ''BinaryOperator
