{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Annotated where

import Control.Lens (makePrisms)
import Data.Functor.Extra ((<<<$>>>))
import Elara.AST.Name (Name (NOpName, NVarName), OpName, Qualified, TypeName, VarName)
import Elara.AST.Region (Located (Located), unlocate)
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
    | LetIn (Located (Qualified VarName)) Expr Expr
    | Let (Located (Qualified VarName)) Expr
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
    deriving (Show, Eq)

newtype Pattern = Pattern (Located Pattern')
    deriving (Show, Eq)

data BinaryOperator'
    = Op (Located (Qualified OpName))
    | Infixed (Located (Qualified VarName))
    deriving (Show, Eq)

operatorName :: BinaryOperator -> Qualified Name
operatorName (MkBinaryOperator (Located _ (Op op))) = NOpName <$> unlocate op
operatorName (MkBinaryOperator (Located _ (Infixed op))) = NVarName <$> unlocate op

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
    deriving (Show, Eq)

makePrisms ''Expr
makePrisms ''Pattern
makePrisms ''BinaryOperator
