{-# LANGUAGE TemplateHaskell #-}

module Elara.AST.Unlocated.Typed where

import Control.Lens hiding (List)
import Control.Lens.Extras (uniplate)
import Data.Data (Data)
import Elara.AST.Name (LowerAlphaName, ModuleName, Name, Qualified (..), TypeName, VarName)
import Elara.Data.Pretty
import Elara.Data.Unique
import Prelude hiding (Op)

data PartialType = Id UniqueId | Partial (Type' PartialType) | Final Type
    deriving (Show, Eq, Ord)

-- | Typed AST Type without location information. See 'Elara.AST.Typed.Expr'' for the location information version.
data Expr' t
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (VarRef VarName)
    | Constructor (VarRef TypeName)
    | Lambda (Unique VarName) (Expr t)
    | FunctionCall (Expr t) (Expr t)
    | If (Expr t) (Expr t) (Expr t)
    | List [Expr t]
    | Match (Expr t) [(Pattern t, Expr t)]
    | LetIn (Unique VarName) (Expr t) (Expr t)
    | Let (Unique VarName) (Expr t)
    | Block (NonEmpty (Expr t))
    | Tuple (NonEmpty (Expr t))
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

newtype Expr t = Expr (Expr' t, t)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

withoutType :: Expr t -> Expr' t
withoutType (Expr (e, _)) = e

data VarRef n
    = Global (Qualified n)
    | Local (Unique n)
    deriving (Show, Eq, Ord, Functor, Data)

data Pattern' t
    = VarPattern (VarRef VarName)
    | ConstructorPattern (Qualified TypeName) [Pattern t]
    | ListPattern [Pattern t]
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

newtype Pattern t = Pattern (Pattern' t, t)
    deriving (Show, Eq, Data, Functor, Foldable, Traversable)

data TypeAnnotation t = TypeAnnotation (Qualified Name) t
    deriving (Show, Eq, Data)

data Type' t
    = TypeVar TypeVar
    | FunctionType t t
    | UnitType
    | TypeConstructorApplication t t
    | UserDefinedType (Qualified TypeName)
    | RecordType (NonEmpty (VarName, t))
    | TupleType (NonEmpty t)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Data)

newtype Type = Type (Type' Type)
    deriving (Show, Eq, Ord, Data)

newtype TypeVar = TyVar (Unique LowerAlphaName)
    deriving (Show, Eq, Ord, Data)

data Declaration t = Declaration'
    { _declaration'Module' :: ModuleName
    , _declaration'Name :: Qualified Name
    , _declaration'Body :: DeclarationBody t
    }
    deriving (Show, Eq)

data DeclarationBody t
    = -- | def <name> : <type> and / or let <p> = <e>
      Value
        { _expression :: Expr t
        , _valueType :: Maybe (TypeAnnotation t)
        }
    | NativeDef (TypeAnnotation t)
    | -- | type <name> = <type>
      TypeAlias Type
    deriving (Show, Eq)

makePrisms ''Declaration
makePrisms ''DeclarationBody
makeLenses ''DeclarationBody
makePrisms ''Expr
makePrisms ''Expr'
makePrisms ''VarRef
makePrisms ''PartialType
makePrisms ''Pattern

instance Pretty Type where
    pretty (Type t) = pretty t

instance (Pretty t) => Pretty (Type' t) where
    pretty (TypeVar (TyVar u)) = pretty (u ^. uniqueVal)
    pretty (FunctionType a b) = pretty a <+> "->" <+> pretty b
    pretty UnitType = "()"
    pretty (TypeConstructorApplication a b) = parens (pretty a <+> pretty b)
    pretty (UserDefinedType q) = pretty q
    pretty (TupleType ts) = "(" <+> hsep (punctuate ", " (pretty <$> toList ts)) <+> ")"
    pretty (RecordType fields) = "{" <+> hsep (punctuate ", " (pprField <$> toList fields)) <+> "}"
      where
        pprField (name, t) = pretty name <+> ":" <+> pretty t

instance (Pretty t) => Pretty (Expr t) where
    pretty (Expr (e@(Block _), _)) = pretty e -- we don't want to print the type of the block
    pretty (Expr (e, t)) = parens (pretty e <+> ":" <+> pretty t)

instance (Pretty t) => Pretty (Expr' t) where
    pretty (Int i) = pretty i
    pretty (Float f) = pretty f
    pretty (String s) = pretty s
    pretty (Char c) = pretty c
    pretty Unit = "()"
    pretty (Var v) = pretty v
    pretty (Constructor c) = pretty c
    pretty (Lambda u e) = "\\" <> pretty u <+> "->" <+> pretty e
    pretty (FunctionCall a b) = pretty a <+> pretty b
    pretty (If c t e) = "if" <+> pretty c <+> "then" <+> pretty t <+> "else" <+> pretty e
    pretty (List es) = "[" <+> hsep (punctuate ", " (pretty <$> es)) <+> "]"
    pretty (Match e cases) =
        vsep
            [ "match" <+> pretty e <+> "with" <+> "{"
            , indent indentDepth (vsep (pprCase <$> cases))
            , "}"
            ]
      where
        pprCase (p, e) = pretty p <+> "->" <+> pretty e
    pretty (LetIn u e1 e2) = "let" <+> pretty u <+> "=" <+> pretty e1 <+> "in" <+> pretty e2
    pretty (Let u e) = "let" <+> pretty u <+> "=" <+> pretty e
    pretty (Block es) = "{" <+> sep (punctuate ";" (pretty <$> toList es)) <+> "}"
    pretty (Tuple es) = "(" <+> hsep (punctuate ", " (pretty <$> toList es)) <+> ")"

instance (Pretty t) => Pretty (Pattern t) where
    pretty (Pattern (p, t)) = parens (pretty p <+> ":" <+> pretty t)

instance (Pretty t) => Pretty (Pattern' t) where
    pretty (VarPattern v) = pretty v
    pretty (ConstructorPattern c ps) = pretty c <+> hsep (pretty <$> ps)
    pretty (ListPattern ps) = "[" <+> hsep (punctuate ", " (pretty <$> ps)) <+> "]"
    pretty WildcardPattern = "_"
    pretty (IntegerPattern i) = pretty i
    pretty (FloatPattern f) = pretty f
    pretty (StringPattern s) = pretty s
    pretty (CharPattern c) = pretty c

instance (Pretty v) => Pretty (VarRef v) where
    pretty (Global q) = pretty q
    pretty (Local u) = pretty u

instance Pretty PartialType where
    pretty (Id u) = pretty u
    pretty (Partial t) = pretty t
    pretty (Final t) = pretty t

instance (Data t) => Plated (Expr t) where
    plate = uniplate

instance (Data t) => Plated (Expr' t) where
    plate = uniplate
