{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Elara.AST.Generic.Types (
    Select,
    Expr' (..),
    Expr (..),
    Pattern' (..),
    Pattern (..),
    Type (..),
    Type' (..),
    BinaryOperator (..),
    BinaryOperator' (..),
    DeclarationBody (..),
    DeclarationBody' (..),
    TypeDeclaration (..),
    Declaration (..),
    Declaration' (..),
    typeOf,
    RUnlocate (..),
    ASTLocate,
    ASTLocate',
    FullASTQual,
    ASTQual,
    CleanupLocated,
    Unlocated (..),
    coerceType,
    coerceType',
    coerceTypeDeclaration,
    pattern Expr',
)
where

import Control.Lens (view)
import Data.Kind qualified as Kind
import Elara.AST.Name (LowerAlphaName, ModuleName)
import Elara.AST.Region (Located, unlocated)
import Elara.AST.Select (LocatedAST, UnlocatedAST)
import GHC.Generics
import GHC.TypeLits
import Prelude hiding (group)

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
    | TypeApplication (Expr ast) (Select "TypeApplication" ast)
    | If (Expr ast) (Expr ast) (Expr ast)
    | BinaryOperator !(Select "BinaryOperator" ast)
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
    | Tuple (NonEmpty (Expr ast))
    deriving (Generic)

newtype Expr (ast :: a) = Expr (ASTLocate ast (Expr' ast), Select "ExprType" ast)
    deriving (Generic, Typeable)

pattern Expr' ::
    forall astK (ast :: astK).
    ( RUnlocate ast
    ) =>
    Expr' ast ->
    Expr ast
pattern Expr' e' <- Expr (rUnlocate @astK @ast -> e', _)

typeOf :: forall ast. Expr ast -> Select "ExprType" ast
typeOf (Expr (_, t)) = t

data Pattern' ast
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

newtype Pattern ast = Pattern (ASTLocate ast (Pattern' ast), Select "PatternType" ast)
    deriving (Generic)

data BinaryOperator' (ast :: a)
    = SymOp (ASTLocate ast (Select "SymOp" ast))
    | Infixed (Select "Infixed" ast)
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
    | RecordType (NonEmpty (ASTLocate ast LowerAlphaName, Type ast))
    | TupleType (NonEmpty (Type ast))
    | ListType (Type ast)
    deriving (Generic)

-- Ttg stuff

type RUnlocate :: ast -> Kind.Constraint
class RUnlocate ast where
    rUnlocate ::
        forall a.
        (CleanupLocated (Located a) ~ Located a) =>
        ASTLocate ast a ->
        a

    fmapUnlocated ::
        forall a b.
        (CleanupLocated (Located a) ~ Located a, CleanupLocated (Located b) ~ Located b) =>
        (a -> b) ->
        ASTLocate ast a ->
        ASTLocate ast b

    traverseUnlocated ::
        forall f a b.
        (Applicative f, CleanupLocated (Located a) ~ Located a, CleanupLocated (Located b) ~ Located b) =>
        (a -> f b) ->
        ASTLocate ast a ->
        f (ASTLocate ast b)

instance (ASTLocate' ast ~ Located) => RUnlocate (ast :: LocatedAST) where
    rUnlocate = view unlocated
    fmapUnlocated = fmap
    traverseUnlocated = traverse

instance (ASTLocate' ast ~ Unlocated) => RUnlocate (ast :: UnlocatedAST) where
    rUnlocate = identity
    fmapUnlocated f = f
    traverseUnlocated f = f

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

coerceTypeDeclaration :: (_) => TypeDeclaration ast1 -> TypeDeclaration ast2
coerceTypeDeclaration (Alias a) = Alias (coerceType a)
coerceTypeDeclaration (ADT a) = ADT (fmap coerceType <<$>> a)

coerceType :: (_) => Type ast1 -> Type ast2
coerceType (Type a) = Type (coerceType' <$> a)

coerceType' :: (_) => Type' ast1 -> Type' ast2
coerceType' (TypeVar a) = TypeVar a
coerceType' (FunctionType a b) = FunctionType (coerceType a) (coerceType b)
coerceType' UnitType = UnitType
coerceType' (TypeConstructorApplication a b) = TypeConstructorApplication (coerceType a) (coerceType b)
coerceType' (UserDefinedType a) = UserDefinedType a
coerceType' (RecordType a) = RecordType (fmap coerceType <$> a)
coerceType' (TupleType a) = TupleType (coerceType <$> a)
coerceType' (ListType a) = ListType (coerceType a)
