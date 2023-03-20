{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Frontend where

import Control.Lens.TH
import Elara.AST.Name (MaybeQualified, ModuleName, Name, OpName, TypeName, Unqualified, VarName)
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
  | Match Expr [(Pattern, Expr)]
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

data TypeAnnotation = TypeAnnotation (Located (MaybeQualified Name)) Type
  deriving (Show, Eq)

data Type
  = TypeVar Text
  | FunctionType Type Type
  | UnitType
  | TypeConstructorApplication Type Type
  | UserDefinedType (Located (MaybeQualified TypeName))
  | RecordType (NonEmpty (Located (Unqualified VarName), Type))
  deriving (Show, Eq)

newtype Declaration = Declaration (Located Declaration')
  deriving (Show, Eq)

data Declaration' = Declaration'
  { _declaration'Module' :: Located ModuleName
  , _declaration'Name :: Located (MaybeQualified Name)
  , _declaration'Body :: DeclarationBody
  }
  deriving (Show, Eq)

newtype DeclarationBody = DeclarationBody (Located DeclarationBody')
  deriving (Show, Eq)

data DeclarationBody'
  = -- | let <p> = <e>
    Value
      { _expression :: Expr
      , _patterns :: [Pattern]
      }
  | -- | def <name> : <type>.
    ValueTypeDef (Located TypeAnnotation)
  | -- | type <name> = <type>
    TypeAlias (Located Type)
  deriving (Show, Eq)

makeLenses ''Declaration'
makeClassy ''Declaration
makeClassy ''DeclarationBody'
makePrisms ''Declaration
makePrisms ''DeclarationBody
makePrisms ''Expr
makePrisms ''Pattern