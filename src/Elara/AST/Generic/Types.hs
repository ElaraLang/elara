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
    TypedLambdaParam (..),
    Type (..),
    Type' (..),
    BinaryOperator (..),
    BinaryOperator' (..),
    DeclarationBody (..),
    DeclarationBody' (..),
    TypeDeclaration (..),
    Declaration (..),
    Declaration' (..),
    AssociativityType (..),
    InfixDeclaration (..),
    ValueDeclAnnotations (..),
    TypeDeclAnnotations (..),
    typeOf,
    freeTypeVars,
    patternTypeOf,
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
    coerceInfixDeclaration,
    pattern Expr',
    exprLocation,
    coerceTypeDeclAnnotations,
    coerceValueDeclAnnotations,
    declarationBody'Name,
    declarationBodyName,
    declaration'Name,
    declarationName,
)
where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Generics.Product (HasField' (field'))
import Data.Generics.Wrapped
import Data.Kind qualified as Kind
import Elara.AST.Generic.Utils
import Elara.AST.Name (ContainsName (..), LowerAlphaName, ModuleName, Name, ToName (..))
import Elara.AST.Region (Located, SourceRegion, sourceRegion, unlocated)
import Elara.AST.Select (LocatedAST, UnlocatedAST)
import GHC.Generics
import GHC.TypeLits
import Prelude hiding (group)

{- | Used to select a field type for a given AST.

Conventions for usage:
If a selection is likely to be one of the "principal" newtypes ('Expr', 'Pattern', etc), it should not be wrapped in 'ASTLocate',
as this increases friction and creates redundant 'Located' wrappers.
This means that implementations should manually wrap in 'Locate' if not using one of the principal newtypes
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
    | List !(Select "List" ast)
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
    | Tuple !(Select "Tuple" ast)
    | InParens !(Select "InParens" ast)
    deriving (Generic)

data InfixDeclaration ast = InfixDeclaration
    { name :: ASTLocate ast (Select "AnyName" ast)
    , prec :: ASTLocate ast Int
    , assoc :: ASTLocate ast AssociativityType
    }
    deriving (Generic)

instance ToName (ASTLocate ast (Select "AnyName" ast)) => ContainsName (InfixDeclaration ast) Name where
    containedName = field' @"name" % Prelude.to toName

data AssociativityType
    = LeftAssoc
    | RightAssoc
    | NonAssoc
    deriving (Generic, Show, Eq, Ord)

newtype Expr (ast :: a) = Expr (ASTLocate ast (Expr' ast), Select "ExprType" ast)
    deriving (Generic, Typeable)

pattern Expr' ::
    forall astK (ast :: astK).
    RUnlocate ast =>
    Expr' ast ->
    Expr ast
pattern Expr' e' <- Expr (rUnlocate @astK @ast -> e', _)

typeOf :: forall ast. Expr ast -> Select "ExprType" ast
typeOf (Expr (_, t)) = t

patternTypeOf :: forall ast. Pattern ast -> Select "PatternType" ast
patternTypeOf (Pattern (_, t)) = t

exprLocation :: ASTLocate' ast ~ Located => Lens' (Expr ast) SourceRegion
exprLocation = _Unwrapped % _1 % sourceRegion

data Pattern' ast
    = VarPattern (ASTLocate ast (Select "VarPat" ast))
    | ConstructorPattern (ASTLocate ast (Select "ConPat" ast)) [Pattern ast]
    | ListPattern !(Select "ListPattern" ast)
    | ConsPattern !(Select "ConsPattern" ast)
    | WildcardPattern
    | IntegerPattern Integer
    | FloatPattern Double
    | StringPattern Text
    | CharPattern Char
    | UnitPattern
    deriving (Generic)

newtype Pattern ast = Pattern (ASTLocate ast (Pattern' ast), Select "PatternType" ast)
    deriving (Generic)

{- | Wrapper over a tuple of a param name and its type
Used mainly to influence Prettyprinting (if we just used a tuple it would get printed as (x,t) rather than x:t)
-}
newtype TypedLambdaParam v ast = TypedLambdaParam (v, Select "PatternType" ast)
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
    , body :: DeclarationBody ast
    }
    deriving (Generic)

newtype DeclarationBody (ast :: a) = DeclarationBody (ASTLocate ast (DeclarationBody' ast))
    deriving (Generic)

data DeclarationBody' (ast :: a)
    = -- | let <p> = <e>
      Value
        { _valueName :: ASTLocate ast (Select "ValueName" ast)
        , _expression :: Expr ast
        , _patterns :: Select "ValuePatterns" ast
        , _valueType :: Select "ValueType" ast
        , _valueAnnotations :: ValueDeclAnnotations ast
        }
    | -- | def <name> : <type>.
      ValueTypeDef
        { _valueName :: ASTLocate ast (Select "ValueName" ast)
        , _valueTypeDef :: !(Select "ValueTypeDef" ast)
        }
    | -- | type <name> <vars> = <type>
      TypeDeclaration
        { _typeDeclarationName :: ASTLocate ast (Select "TypeName" ast)
        , typeVars :: [ASTLocate ast (Select "TypeVar" ast)]
        , typeDeclarationBody :: ASTLocate ast (TypeDeclaration ast)
        , typeAnnotations :: TypeDeclAnnotations ast
        }
    | -- | infix[l/r] <prec> <name>
      InfixDecl !(Select "InfixDecl" ast)
    deriving (Generic)

declarationBody'Name ::
    forall ast.
    _ =>
    Getter (DeclarationBody' ast) (ASTLocate ast Name)
declarationBody'Name = Prelude.to $ \case
    Value n _ _ _ _ -> fmapUnlocated @_ @ast @(Select "ValueName" ast) @Name toName n
    ValueTypeDef n _ -> fmapUnlocated @_ @ast @(Select "ValueName" ast) @Name toName n
    TypeDeclaration n _ _ _ -> fmapUnlocated @_ @ast @(Select "TypeName" ast) @Name toName n
    InfixDecl decl ->
        let (InfixDeclaration n _ _) = dataConAs @(Select "InfixDecl" ast) @(InfixDeclaration ast) decl
         in fmapUnlocated @_ @ast @(Select "AnyName" ast) @Name toName n

declarationBodyName ::
    forall ast.
    _ =>
    Getter (DeclarationBody ast) (ASTLocate ast Name)
declarationBodyName = _Unwrapped % rUnlocated @_ @ast % declarationBody'Name @ast

declaration'Name ::
    forall ast.
    _ =>
    Getter (Declaration' ast) (ASTLocate ast Name)
declaration'Name = field' @"body" % declarationBodyName @ast

declarationName ::
    forall ast.
    _ =>
    Getter (Declaration ast) (ASTLocate ast Name)
declarationName = _Unwrapped % rUnlocated @_ @ast % declaration'Name @ast

newtype ValueDeclAnnotations ast = ValueDeclAnnotations
    { infixValueDecl :: Maybe (InfixDeclaration ast)
    }

data TypeDeclAnnotations ast = TypeDeclAnnotations
    { infixTypeDecl :: Maybe (InfixDeclaration ast)
    , kindAnn :: !(Select "KindAnnotation" ast)
    }

data TypeDeclaration ast
    = ADT
        ( NonEmpty
            -- Non-empty list of constructors
            ( ASTLocate ast (Select "ConstructorName" ast) -- Constructor name
            , [Select "ADTParam" ast] -- Constructor parameters
            )
        )
    | Alias !(Select "Alias" ast)
    deriving (Generic)

newtype Type ast = Type (ASTLocate ast (Type' ast), Select "TypeKind" ast)
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

freeTypeVars :: _ => Type ast -> [ASTLocate ast (Select "TypeVar" ast)]
freeTypeVars =
    nubOrdOn (view unlocated) -- remove duplicates, ignore location info when comparing
        . concatMapOf (cosmosOnOf (_Unwrapped % _1 % unlocated) gplate) names
  where
    names = \case
        TypeVar l -> [l]
        _ -> [] -- cosmos takes care of the recursion :D

-- Ttg stuff

type RUnlocate :: ast -> Kind.Constraint
class RUnlocate ast where
    rUnlocate ::
        forall a.
        CleanupLocated (Located a) ~ Located a =>
        ASTLocate ast a ->
        a
    rUnlocate = view (rUnlocated @_ @ast @a)

    rUnlocated :: forall a. CleanupLocated (Located a) ~ Located a => Getter (ASTLocate ast a) a

    fmapUnlocated ::
        forall a b.
        (CleanupLocated (Located a) ~ Located a, CleanupLocated (Located b) ~ Located b) =>
        (a -> b) ->
        ASTLocate ast a ->
        ASTLocate ast b

    traverseUnlocated ::
        forall a b.
        (CleanupLocated (Located a) ~ Located a, CleanupLocated (Located b) ~ Located b) =>
        Traversal (ASTLocate ast a) (ASTLocate ast b) a b

instance ASTLocate' ast ~ Located => RUnlocate (ast :: LocatedAST) where
    rUnlocated = castOptic unlocated
    fmapUnlocated = fmap
    traverseUnlocated = traversalVL traverse

instance ASTLocate' ast ~ Unlocated => RUnlocate (ast :: UnlocatedAST) where
    rUnlocated = Prelude.to identity
    fmapUnlocated f = f
    traverseUnlocated = traversalVL identity

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
coerceType (Type (a, kind)) = Type (coerceType' <$> a, kind)

coerceType' :: _ => Type' ast1 -> Type' ast2
coerceType' (TypeVar a) = TypeVar a
coerceType' (FunctionType a b) = FunctionType (coerceType a) (coerceType b)
coerceType' UnitType = UnitType
coerceType' (TypeConstructorApplication a b) = TypeConstructorApplication (coerceType a) (coerceType b)
coerceType' (UserDefinedType a) = UserDefinedType a
coerceType' (RecordType a) = RecordType (fmap coerceType <$> a)
coerceType' (TupleType a) = TupleType (coerceType <$> a)
coerceType' (ListType a) = ListType (coerceType a)

coerceValueDeclAnnotations :: _ => ValueDeclAnnotations ast1 -> ValueDeclAnnotations ast2
coerceValueDeclAnnotations (ValueDeclAnnotations v) = ValueDeclAnnotations (coerceInfixDeclaration <$> v)

coerceTypeDeclAnnotations :: _ => TypeDeclAnnotations ast1 -> TypeDeclAnnotations ast2
coerceTypeDeclAnnotations (TypeDeclAnnotations v k) = TypeDeclAnnotations (coerceInfixDeclaration <$> v) k

coerceInfixDeclaration ::
    _ =>
    InfixDeclaration ast1 ->
    InfixDeclaration ast2
coerceInfixDeclaration (InfixDeclaration n a b) = InfixDeclaration n a b
