{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Renamed where

import Control.Lens.TH
import Elara.AST.Name (MaybeQualified, ModuleName, Name, OpName, TypeName, VarName)
import Elara.AST.Region (Located)
import Elara.Data.Unique (Unique)
import Prelude hiding (Type)

-- | Renamed AST. Identical to the frontend AST, except that all names are renamed to be unique
data Expr'
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (Located (Unique (MaybeQualified VarName)))
    | Constructor (Located (Unique (MaybeQualified TypeName)))
    | Lambda [Pattern] Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | BinaryOperator BinaryOperator Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn (Located (Unique  (MaybeQualified VarName))) [Pattern] Expr Expr
    | Let (Located (Unique (MaybeQualified VarName))) [Pattern] Expr
    | Block (NonEmpty Expr)
    | InParens Expr
    deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
    deriving (Show, Eq)

data Pattern'
    = VarPattern (Located (Unique VarName))
    | ConstructorPattern (Located (Unique (MaybeQualified TypeName))) [Pattern]
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
    = Op (Located (Unique (MaybeQualified OpName)))
    | Infixed (Located (Unique (MaybeQualified (VarName))))
    deriving (Show, Eq)

newtype BinaryOperator = MkBinaryOperator (Located BinaryOperator')
    deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation (Located (Unique Name)) Type
    deriving (Show, Eq)

data Type
    = TypeVar Text
    | FunctionType Type Type
    | UnitType
    | TypeConstructorApplication Type Type
    | UserDefinedType (Located (Unique (MaybeQualified TypeName)))
    | RecordType (NonEmpty (Located VarName, Type))
    deriving (Show, Eq)

newtype Declaration = Declaration (Located Declaration')
    deriving (Show, Eq)

data Declaration' = Declaration'
    { _declaration'Module' :: Located ModuleName
    , _declaration'Name :: Located (Unique Name)
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
