{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Renamed where

import Control.Lens (makeLenses, makePrisms, view)
import Elara.AST.Name (HasName (name), LowerAlphaName, ModuleName, Name, OpName, Qualified, TypeName, VarName)
import Elara.AST.Region (Located)
import Elara.Data.Unique
import Prelude hiding (Op, Type)

{- | Renamed AST Type
This is very similar to 'Elara.AST.Desugared.Expr'' except everything is renamed to be unambiguous.
-}
data Expr'
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (Located (VarRef VarName))
    | Constructor (Located (VarRef TypeName))
    | Lambda (Located (Unique VarName)) Expr
    | FunctionCall Expr Expr
    | If Expr Expr Expr
    | BinaryOperator BinaryOperator Expr Expr
    | List [Expr]
    | Match Expr [(Pattern, Expr)]
    | LetIn (Located (Unique VarName)) Expr Expr
    | Let (Located (Unique VarName)) Expr
    | Block (NonEmpty Expr)
    | InParens Expr
    | Tuple (NonEmpty Expr)
    deriving (Show, Eq)

newtype Expr = Expr (Located Expr')
    deriving (Show, Eq)

data VarRef n
    = Global (Located (Qualified n))
    | Local (Located (Unique n))
    deriving (Show, Eq, Ord, Functor)

varRefVal :: VarRef n -> Located n
varRefVal (Global n) = fmap (view name) n
varRefVal (Local n) = fmap (view uniqueVal) n

data Pattern'
    = VarPattern (Located (VarRef VarName))
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
    = Op (Located (VarRef OpName))
    | Infixed (Located (VarRef VarName))
    deriving (Show, Eq, Ord)

newtype BinaryOperator = MkBinaryOperator (Located BinaryOperator')
    deriving (Show, Eq, Ord)

data TypeAnnotation = TypeAnnotation (Located (Qualified Name)) Type
    deriving (Show, Eq)

data Type
    = TypeVar (Unique LowerAlphaName)
    | FunctionType Type Type
    | UnitType
    | TypeConstructorApplication Type Type
    | UserDefinedType (Located (Qualified TypeName))
    | RecordType (NonEmpty (Located VarName, Type))
    deriving (Show, Eq)

newtype Declaration = Declaration (Located Declaration')
    deriving (Show, Eq)

data Declaration' = Declaration'
    { _declaration'Module' :: Located ModuleName
    , _declaration'Name :: Located (Qualified Name)
    , _declaration'Body :: DeclarationBody
    }
    deriving (Show, Eq)

newtype DeclarationBody = DeclarationBody (Located DeclarationBody')
    deriving (Show, Eq)

data DeclarationBody'
    = -- | def <name> : <type> and / or let <p> = <e>
      Value
        { _expression :: Expr
        , _valueType :: Maybe (Located TypeAnnotation)
        }
    | -- | type <name> = <type>
      TypeAlias (Located Type)
    deriving (Show, Eq)

makePrisms ''Declaration
makeLenses ''Declaration'
makePrisms ''DeclarationBody
makePrisms ''DeclarationBody'
makeLenses ''DeclarationBody
makeLenses ''DeclarationBody'
makePrisms ''Expr
makePrisms ''Pattern
makePrisms ''BinaryOperator
