{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Typed where

import Control.Lens (makeLenses, makePrisms)
import Elara.AST.Name (ModuleName, Name, Qualified, TypeName, VarName)
import Elara.AST.Region (Located)
import Elara.Data.Unique
import Prelude hiding (Op, Type)

data PartialType = Id Int | Final Type

{- | Typed AST Type
This is very similar to 'Elara.AST.Shunted.Expr' except:

- Everything has a type!
-}
data Expr' t
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (Located (VarRef VarName))
    | Constructor (Located (VarRef TypeName))
    | Lambda Pattern (Expr t)
    | FunctionCall (Expr t) (Expr t)
    | If (Expr t) (Expr t) (Expr t)
    | List [Expr t]
    | Match (Expr t) [(Pattern, Expr t)]
    | LetIn (Located (Unique VarName)) (Expr t) (Expr t)
    | Let (Located (Unique VarName)) (Expr t)
    | Block (NonEmpty (Expr t))
    deriving (Show, Eq)

newtype Expr t = Expr (Located (Expr' t), t)
    deriving (Show, Eq)

data VarRef n
    = Global (Located (Qualified n))
    | Local (Located (Unique n))
    deriving (Show, Eq, Ord, Functor)

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

data TypeAnnotation = TypeAnnotation (Located (Qualified Name)) Type
    deriving (Show, Eq)

data Type
    = TypeVar TypeVar
    | FunctionType Type Type
    | UnitType
    | TypeConstructorApplication Type Type
    | UserDefinedType (Located (Qualified TypeName))
    | RecordType (NonEmpty (Located VarName, Type))
    deriving (Show, Eq)

newtype TypeVar = TyVar (Unique Text)
    deriving (Show, Eq, Ord)

newtype Declaration t = Declaration (Located (Declaration' t))
    deriving (Show, Eq)

data Declaration' t = Declaration'
    { _declaration'Module' :: Located ModuleName
    , _declaration'Name :: Located (Qualified Name)
    , _declaration'Body :: DeclarationBody t
    }
    deriving (Show, Eq)

newtype DeclarationBody t = DeclarationBody (Located (DeclarationBody' t))
    deriving (Show, Eq)

data DeclarationBody' t
    = -- | def <name> : <type> and / or let <p> = <e>
      Value
        { _expression :: Expr t
        , _valueType :: Maybe (Located TypeAnnotation)
        }
    | NativeDef (Located TypeAnnotation)
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
