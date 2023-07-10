{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Frontend where

import Control.Lens.TH
import Elara.AST.Name (LowerAlphaName, MaybeQualified, ModuleName, Name, OpName, TypeName, VarName)
import Elara.AST.Region (Located)

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
  | Match Expr [(Pattern, Expr)]
  | LetIn (Located VarName) [Pattern] Expr Expr
  | Let (Located VarName) [Pattern] Expr
  | Block (NonEmpty Expr)
  | InParens Expr
  | Tuple (NonEmpty Expr)
  deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
  deriving (Show, Eq)

data Pattern'
  = VarPattern (Located VarName)
  | ConstructorPattern (Located (MaybeQualified TypeName)) [Pattern]
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
  = Op (Located (MaybeQualified OpName))
  | Infixed (Located (MaybeQualified VarName))
  deriving (Show, Eq)

newtype BinaryOperator = MkBinaryOperator (Located BinaryOperator')
  deriving (Show, Eq)

data Type
  = TypeVar (Located LowerAlphaName)
  | FunctionType (Located Type) (Located Type)
  | UnitType
  | TypeConstructorApplication (Located Type) (Located Type)
  | UserDefinedType (Located (MaybeQualified TypeName))
  | RecordType (NonEmpty (Located VarName, Located Type))
  | TupleType (NonEmpty (Located Type))
  deriving (Show, Eq)

newtype Declaration = Declaration (Located Declaration')
  deriving (Show, Eq)

data Declaration' = Declaration'
  { _declaration'Module' :: Located ModuleName,
    _declaration'Name :: Located Name,
    _declaration'Body :: Located DeclarationBody
  }
  deriving (Show, Eq)

newtype DeclarationBody = DeclarationBody (Located DeclarationBody')
  deriving (Show, Eq)

data DeclarationBody'
  = -- | let <p> = <e>
    Value
      { _expression :: Expr,
        _patterns :: [Pattern]
      }
  | -- | def <name> : <type>.
    ValueTypeDef (Located Type)
  | -- | type <name> <vars> = <type>
    TypeDeclaration [Located LowerAlphaName] (Located TypeDeclaration)
  deriving (Show, Eq)

data TypeDeclaration
  = ADT (NonEmpty (Located TypeName, [Located Type]))
  | Alias (Located Type)
  deriving (Show, Eq)

makeLenses ''Declaration'
makeClassy ''Declaration
makeClassy ''DeclarationBody'
makePrisms ''Declaration
makePrisms ''DeclarationBody
makePrisms ''TypeDeclaration
makePrisms ''Expr
makePrisms ''Pattern
