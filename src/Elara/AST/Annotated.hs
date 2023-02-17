{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Annotated where

import Control.Lens (makePrisms)
import Elara.AST.Name (Name (NOpName, NVarName), OpName, Qualified, TypeName, VarName)
import Elara.AST.Region (Located (Located))
import Elara.Data.Type (Type)
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
    | Var (Qualified VarName)
    | Constructor (Qualified TypeName)
    | Lambda Pattern Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | BinaryOperator BinaryOperator Expr Expr
    | List [Expr]
    | LetIn (Qualified VarName) Expr Expr
    | Let (Qualified VarName) Expr
    | Block (NonEmpty Expr)
    | InParens Expr
    deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
    deriving (Show, Eq)

data Pattern'
    = NamedPattern Text
    | ConstructorPattern (Qualified TypeName) [Pattern]
    | ListPattern [Pattern]
    | WildcardPattern
    deriving (Show, Eq)

newtype Pattern = Pattern (Located Pattern')
    deriving (Show, Eq)

data BinaryOperator'
    = Op (Qualified OpName)
    | Infixed (Qualified VarName)
    deriving (Show, Eq)
    
operatorName :: BinaryOperator -> Name Qualified
operatorName (MkBinaryOperator (Located _ (Op op))) = NOpName op
operatorName (MkBinaryOperator (Located _ (Infixed op))) = NVarName op

newtype BinaryOperator = MkBinaryOperator (Located BinaryOperator')
    deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation (Name Qualified) (Type Qualified)
    deriving (Show, Eq)

makePrisms ''Expr
makePrisms ''Pattern
makePrisms ''BinaryOperator