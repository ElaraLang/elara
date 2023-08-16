{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Elara.AST.Generic where

-- import Elara.AST.Frontend qualified as Frontend

import Control.Lens (to, view, (^.))
import Data.Data (Data)
import Data.Generics.Wrapped
import Data.Kind qualified as Kind
import Elara.AST.Name (ModuleName, Name, Qualified, VarName (..))
import Elara.AST.Pretty
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Select (AST (..), LocatedAST, UnlocatedAST)
import Elara.Data.Pretty
import GHC.Generics (
    C,
    D,
    Generic (Rep),
    M1,
    Meta (MetaCons),
    type (:+:),
 )
import GHC.TypeLits
import Prelude hiding (group)

data DataConCantHappen deriving (Generic, Data)

dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

data NoFieldValue = NoFieldValue
    deriving (Generic, Data)

{- | Used to select a field type for a given AST.

Conventions for usage:
If a selection is likely to be one of the "principal" newtypes ('Expr', 'Pattern', etc), it should not be wrapped in 'ASTLocate',
as this increases friction and creates redundant 'Located' wrappers.
This means that implementations should manually wrap in 'Locate' if not using one of the principle newtypes
-}
type family Select (s :: Symbol) (ast :: a) = (v :: Kind.Type)

data Expr' (ast :: a)
    = Int Integer
    | Float Double
    | String Text
    | Char Char
    | Unit
    | Var (ASTLocate ast (Select "VarRef" ast))
    | Constructor (ASTLocate ast (Select "ConRef" ast))
    | Lambda
        (ASTLocate ast (Select "LambdaPattern" ast))
        (Expr ast)
    | FunctionCall (Expr ast) (Expr ast)
    | If (Expr ast) (Expr ast) (Expr ast)
    | BinaryOperator (BinaryOperator ast) (Expr ast) (Expr ast)
    | List [Expr ast]
    | Match (Expr ast) [(Pattern ast, Expr ast)]
    | LetIn
        (ASTLocate ast (Select "LetParamName" ast))
        (Select "LetPattern" ast)
        (Expr ast)
        (Expr ast)
    | Let
        (ASTLocate ast (Select "LetParamName" ast))
        (Select "LetPattern" ast)
        (Expr ast)
    | Block (NonEmpty (Expr ast))
    | InParens !(Select "InParens" ast)
    | Tuple (NonEmpty (Expr ast))
    deriving (Generic)

newtype Expr (ast :: a) = Expr (ASTLocate ast (Expr' ast), Select "ExprType" ast)
    deriving (Generic)

data Pattern' (ast :: a)
    = VarPattern (ASTLocate ast (Select "VarPat" ast))
    | ConstructorPattern (ASTLocate ast (Select "ConPat" ast)) [Pattern ast]
    | ListPattern [Pattern ast]
    | ConsPattern (Pattern ast) (Pattern ast)
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    | UnitPattern
    deriving (Generic)

newtype Pattern (ast :: a) = Pattern (ASTLocate ast (Pattern' ast), Select "PatternType" ast)
    deriving (Generic)

data BinaryOperator' (ast :: a)
    = SymOp (ASTLocate ast (Select "SymOp" ast))
    | Infixed (ASTLocate ast (Select "Infixed" ast))
    deriving (Generic)

newtype BinaryOperator ast = MkBinaryOperator (ASTLocate ast (BinaryOperator' ast))
    deriving (Generic)

newtype Declaration ast = Declaration (ASTLocate ast (Declaration' ast))
    deriving (Generic)

data Declaration' (ast :: a) = Declaration'
    { moduleName :: ASTLocate ast ModuleName
    , name :: ASTLocate ast (Select "DeclarationName" ast)
    , body :: DeclarationBody ast
    }
    deriving (Generic)

newtype DeclarationBody (ast :: a) = DeclarationBody (ASTLocate ast (DeclarationBody' ast))
    deriving (Generic)

data DeclarationBody' (ast :: a)
    = -- | let <p> = <e>
      Value
        { _expression :: Expr ast
        , _patterns :: Select "ValuePatterns" ast
        , _valueType :: Select "ValueType" ast
        }
    | -- | def <name> : <type>.
      ValueTypeDef !(Select "ValueTypeDef" ast)
    | -- | type <name> <vars> = <type>
      TypeDeclaration
        [ASTLocate ast (Select "TypeVar" ast)]
        (ASTLocate ast (TypeDeclaration ast))
    deriving (Generic)

data TypeDeclaration ast
    = ADT (NonEmpty (ASTLocate ast (Select "ConstructorName" ast), [Type ast]))
    | Alias (Type ast)
    deriving (Generic)

newtype Type ast = Type (ASTLocate ast (Type' ast))
    deriving (Generic)

data Type' ast
    = TypeVar (ASTLocate ast (Select "TypeVar" ast))
    | FunctionType (Type ast) (Type ast)
    | UnitType
    | TypeConstructorApplication (Type ast) (Type ast)
    | UserDefinedType (ASTLocate ast (Select "UserDefinedType" ast))
    | RecordType (NonEmpty (ASTLocate ast VarName, Type ast))
    | TupleType (NonEmpty (Type ast))
    | ListType (Type ast)
    deriving (Generic)

-- Ttg stuff

type RUnlocate :: ast -> Kind.Constraint
class RUnlocate ast where
    rUnlocate ::
        forall a.
        CleanupLocated (Located a) ~ Located a =>
        ASTLocate ast a ->
        a

instance (ASTLocate' ast ~ Located) => RUnlocate (ast :: LocatedAST) where
    rUnlocate = view unlocated

instance (ASTLocate' ast ~ Unlocated, Pretty a20, Pretty a1, Pretty a1, Pretty a20, Pretty a1) => RUnlocate (ast :: UnlocatedAST) where
    rUnlocate = identity

type ASTLocate :: a -> Kind.Type -> Kind.Type
type ASTLocate ast a = CleanupLocated (ASTLocate' ast a)

type FullASTQual :: a -> Kind.Type -> Kind.Type
type FullASTQual ast a = ((ASTLocate ast) (ASTQual ast a))

newtype Unlocated a = Unlocated a

-- | Unwraps a single layer of 'Unlocated' from a type.
type family CleanupLocated g where
    CleanupLocated (Unlocated a) = a
    CleanupLocated (Located (Located a)) = CleanupLocated a
    -- Remove located wrappers for the newtypes
    CleanupLocated (Located (Expr a)) = CleanupLocated (Expr a)
    CleanupLocated (Located (Pattern a)) = CleanupLocated (Pattern a)
    CleanupLocated (Located (BinaryOperator a)) = CleanupLocated (BinaryOperator a)
    CleanupLocated (Located (Declaration a)) = CleanupLocated (Declaration a)
    CleanupLocated (Located (DeclarationBody a)) = CleanupLocated (DeclarationBody a)
    CleanupLocated (Located (Type a)) = CleanupLocated (Type a)
    CleanupLocated a = a

type family ASTLocate' (ast :: a) :: Kind.Type -> Kind.Type

type family ASTQual (ast :: a) :: Kind.Type -> Kind.Type

-- Coercions

coerceTypeDeclaration :: _ => TypeDeclaration ast1 -> TypeDeclaration ast2
coerceTypeDeclaration (Alias a) = Alias (coerceType a)
coerceTypeDeclaration (ADT a) = ADT (fmap coerceType <<$>> a)

coerceType :: _ => Type ast1 -> Type ast2
coerceType (Type a) = Type (coerceType' <$> a)

coerceType' :: _ => Type' ast1 -> Type' ast2
coerceType' (TypeVar a) = TypeVar a
coerceType' (FunctionType a b) = FunctionType (coerceType a) (coerceType b)
coerceType' UnitType = UnitType
coerceType' (TypeConstructorApplication a b) = TypeConstructorApplication (coerceType a) (coerceType b)
coerceType' (UserDefinedType a) = UserDefinedType a
coerceType' (RecordType a) = RecordType (fmap coerceType <$> a)
coerceType' (TupleType a) = TupleType (coerceType <$> a)
coerceType' (ListType a) = ListType (coerceType a)

-- Pretty printing

deriving newtype instance Pretty (ASTLocate ast (BinaryOperator' ast)) => Pretty (BinaryOperator ast)
deriving newtype instance Eq (ASTLocate ast (BinaryOperator' ast)) => Eq (BinaryOperator ast)
deriving newtype instance Ord (ASTLocate ast (BinaryOperator' ast)) => Ord (BinaryOperator ast)
deriving newtype instance Show (ASTLocate ast (BinaryOperator' ast)) => Show (BinaryOperator ast)

deriving instance
    ( Eq (ASTLocate ast (Select "SymOp" ast))
    , Eq (ASTLocate ast (Select "Infixed" ast))
    ) =>
    Eq (BinaryOperator' ast)

deriving instance
    ( Ord (ASTLocate ast (Select "SymOp" ast))
    , Ord (ASTLocate ast (Select "Infixed" ast))
    ) =>
    Ord (BinaryOperator' ast)

deriving newtype instance Pretty (ASTLocate ast (Type' ast)) => Pretty (Type ast)
deriving newtype instance Show (ASTLocate ast (Type' ast)) => Show (Type ast)

deriving instance
    ( Show (ASTLocate ast (Select "TypeVar" ast))
    , Show (ASTLocate ast (Select "UserDefinedType" ast))
    , Show (ASTLocate ast (Type' ast))
    , Show (ASTLocate ast VarName)
    ) =>
    Show (Type' ast)

instance
    ( Pretty (Expr ast)
    , Pretty (ASTLocate ast (Declaration' ast))
    , Pretty (CleanupLocated (ASTLocate' ast (Select "TypeVar" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (Select "DeclarationName" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (TypeDeclaration ast)))
    , Pretty a3
    , ToMaybe (Select "ValuePatterns" ast) a3
    , RUnlocate (ast :: b)
    ) =>
    Pretty (Declaration ast)
    where
    pretty (Declaration ldb) = pretty ldb

instance
    ( Pretty (Expr ast)
    , Pretty (CleanupLocated (ASTLocate' ast (Select "TypeVar" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (Select "DeclarationName" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (TypeDeclaration ast)))
    , Pretty a3
    , ToMaybe (Select "ValuePatterns" ast) a3
    , RUnlocate (ast :: b)
    ) =>
    Pretty (Declaration' ast)
    where
    pretty (Declaration' _ n b) =
        let val = b ^. _Unwrapped :: ASTLocate ast (DeclarationBody' ast)
            y = rUnlocate @b @ast @(DeclarationBody' ast) val
         in prettyDB n y
      where
        prettyDB n (Value e t _) = prettyValueDeclaration n e (toMaybe t :: Maybe a3)
        prettyDB n (TypeDeclaration vars t) = prettyTypeDeclaration n vars t

instance Pretty (TypeDeclaration ast) where
    pretty _ = "TODO"

class ToMaybe i o where
    toMaybe :: i -> Maybe o

instance ToMaybe NoFieldValue a where
    toMaybe _ = Nothing

instance ToMaybe (Maybe a) a where
    toMaybe = identity

instance
    ( Pretty (CleanupLocated (ASTLocate' ast (Expr' ast)))
    , Pretty a1
    , ToMaybe (Select "ExprType" ast) a1
    ) =>
    Pretty (Expr ast)
    where
    pretty (Expr (e, t)) = group (flatAlt long short)
      where
        te = (":" <+>) . pretty <$> (toMaybe t :: Maybe a1)
        long = pretty e <+> pretty te
        short = align (pretty e <+> pretty te)

instance
    ( Pretty (Expr ast)
    , Pretty (CleanupLocated (ASTLocate' ast (Select "LambdaPattern" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (Select "ConRef" ast)))
    , Pretty (CleanupLocated (ASTLocate' ast (Select "VarRef" ast)))
    , Pretty (Pattern ast)
    , (Pretty (Select "InParens" ast))
    , (Pretty (CleanupLocated (ASTLocate' ast (Select "LetParamName" ast))))
    , Pretty a2
    , (ToMaybe (Select "LetPattern" ast) a2)
    ) =>
    Pretty (Expr' ast)
    where
    pretty (Int i) = pretty i
    pretty (Float f) = pretty f
    pretty (String s) = pretty '\"' <> pretty s <> pretty '\"'
    pretty (Char c) = "'" <> escapeChar c <> "'"
    pretty Unit = "()"
    pretty (Var v) = pretty v
    pretty (Constructor c) = pretty c
    pretty (Lambda ps e) = prettyLambdaExpr [ps] e
    pretty (FunctionCall e1 e2) = prettyFunctionCallExpr e1 e2
    pretty (If e1 e2 e3) = prettyIfExpr e1 e2 e3
    pretty (List l) = prettyListExpr l
    pretty (Match e m) = prettyMatchExpr e (prettyMatchBranch <$> m)
    pretty (LetIn v p e1 e2) = prettyLetInExpr v (maybeToList $ toMaybe p :: [a2]) e1 (Just e2)
    pretty (Let v p e) = prettyLetExpr v (maybeToList $ toMaybe p :: [a2]) e
    pretty (Block b) = prettyBlockExpr b
    pretty (InParens e) = parens (pretty e)

instance
    ( Pretty (Pattern' ast)
    , Pretty a1
    , ToMaybe (Select "PatternType" ast) a1
    , (Pretty (CleanupLocated (ASTLocate' ast (Pattern' ast))))
    ) =>
    Pretty (Pattern ast)
    where
    pretty (Pattern (p, t)) = group (flatAlt long short)
      where
        te = (":" <+>) . pretty <$> (toMaybe t :: Maybe a1)
        long = pretty p <+> pretty te
        short = align (pretty p <+> pretty te)

instance (Pretty (CleanupLocated (ASTLocate' ast (Select "VarPat" ast))), Pretty (Pattern ast), Pretty (CleanupLocated (ASTLocate' ast (Select "ConPat" ast)))) => Pretty (Pattern' ast) where
    pretty (VarPattern v) = pretty v
    pretty (ConstructorPattern c ps) = prettyConstructorPattern c ps
    pretty (ListPattern l) = prettyListPattern l
    pretty (ConsPattern p1 p2) = prettyConsPattern p1 p2
    pretty other = error "aaaaaaa"

instance
    ( Pretty (ASTLocate ast (Type' ast))
    , Pretty (ASTLocate ast VarName)
    , Pretty (ASTLocate ast (Select "TypeVar" ast))
    , Pretty (ASTLocate ast (Select "UserDefinedType" ast))
    ) =>
    Pretty (Type' ast)
    where
    pretty = \case
        TypeVar name -> pretty name
        FunctionType a b -> parens (pretty a <+> "->" <+> pretty b)
        UnitType -> "()"
        TypeConstructorApplication a b -> pretty a <+> pretty b
        UserDefinedType name -> pretty name
        RecordType fields -> "{" <+> prettyFields fields <+> "}"
        TupleType fields -> tupled (map pretty (toList fields))
        ListType a -> "[" <+> pretty a <+> "]"
      where
        prettyFields = hsep . punctuate "," . map (\(name, value) -> pretty name <+> ":" <+> pretty value) . toList

type ForAllX c x =
    ( AllX c (CNames (Expr x)) x
    , AllX c (CNames (Expr' x)) x
    , AllX c (CNames (Pattern x)) x
    , AllX c (CNames (Pattern' x)) x
    , AllX c (CNames (BinaryOperator x)) x
    , AllX c (CNames (BinaryOperator' x)) x
    )

type CNames a = GCNames (Rep a)

type family GCNames (f :: Kind.Type -> Kind.Type) :: [Symbol] where
    GCNames (M1 D c f) = GCNames f
    GCNames (f :+: g) = GCNames f ++ GCNames g
    GCNames (M1 C ('MetaCons name _ _) f) = '[name]

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

type family AllX (c :: Kind.Type -> Constraint) (xs :: [Symbol]) (x :: a) :: Constraint where
    AllX c '[] x = ()
    AllX c (s ': ss) x = (c (Select s x), AllX c ss x)