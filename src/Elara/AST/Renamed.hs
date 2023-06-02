{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Renamed where

import Control.Lens (Plated, makeLenses, makePrisms)
import Data.Data (Data)
import Elara.AST.Name (LowerAlphaName, ModuleName, Name, OpName, Qualified, TypeName, VarName)
import Elara.AST.Region (Located (Located))
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Data.Unique
import Prelude hiding (Op)

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
    | Constructor (Located (Qualified TypeName))
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

data Type
    = TypeVar (Located (Unique LowerAlphaName))
    | FunctionType (Located Type) (Located Type)
    | UnitType
    | TypeConstructorApplication (Located Type) (Located Type)
    | UserDefinedType (Located (Qualified TypeName))
    | RecordType (NonEmpty (Located VarName, Located Type))
    | TupleType (NonEmpty (Located Type))
    deriving (Show, Eq, Data)

instance Plated Type

instance Pretty Type where
    pretty = \case
        TypeVar (Located _ name) -> pretty name
        FunctionType a b -> pretty a <+> "->" <+> pretty b
        UnitType -> "()"
        TypeConstructorApplication a b -> pretty a <+> pretty b
        UserDefinedType (Located _ name) -> pretty name
        RecordType fields -> "{" <+> prettyFields fields <+> "}"
        TupleType fields -> tupled (map pretty (toList fields))
      where
        prettyFields = hsep . punctuate "," . map (\(Located _ name, Located _ value) -> pretty name <+> ":" <+> pretty value) . toList

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
        , _valueType :: Maybe (Located Type)
        }
    | -- | type <name> <vars> = <type>
      TypeDeclaration [Located (Unique LowerAlphaName)] (Located TypeDeclaration)
    deriving (Show, Eq)

data TypeDeclaration
    = ADT (NonEmpty (Located (Qualified TypeName), [Located Type]))
    | Alias (Located Type)
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
