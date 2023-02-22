{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Module where

import Control.Lens (Lens, makeFields, makeLenses, makePrisms)
import Control.Lens.Traversal
import Elara.AST.Name (ModuleName, Name, OpName, TypeName, VarName)
import Elara.AST.Region (Located, _Unlocate)
import Elara.AST.Select (ASTAnnotation, ASTExpr, ASTPattern, ASTQual, ASTLocate, FullASTQual)
import Elara.Data.Type
import Prelude hiding (Type)
import Prelude qualified as Kind (Type)

newtype Module ast = Module (Located (Module' ast))

data Module' ast = Module'
    { _module'Name :: ASTLocate ast ModuleName
    , _module'Exposing :: Exposing ast
    , _module'Imports :: [Import ast]
    , _module'Declarations :: [Declaration ast]
    }

-- moduleDeclarations :: (ASTQual ast ~ ASTQual ast2) => Lens (Module' ast) (Module' ast2) [Declaration ast] [Declaration ast2]
-- moduleDeclarations f (Module' n e i d) = fmap (Module' n e i) (f d)


newtype Declaration ast = Declaration (Located (Declaration' ast))
data Declaration' ast = Declaration'
    { _declarationModule' :: ASTLocate ast ModuleName
    , _declarationName :: FullASTQual ast Name
    , _declarationBody :: DeclarationBody ast
    }

-- _declarationBodyLens ::
--     (ASTQual ast ~ ASTQual ast2) =>
--     Lens (Declaration' ast) (Declaration' ast2) (DeclarationBody ast) (DeclarationBody ast2)
-- _declarationBodyLens f (Declaration' m n b) = fmap (Declaration' m n) (f b)

newtype DeclarationBody ast = DeclarationBody (Located (DeclarationBody' ast))
data DeclarationBody' ast
    = -- | let <p> = <e>
      Value
        { _declarationBodyExpression :: ASTExpr ast
        , _declarationBodyPatterns :: [ASTPattern ast]
        -- ^ The patterns used in things like let f x = ...
        , _declarationBodyTypeAnnotation :: ASTAnnotation ast
        }
    | -- | def <name> : <type>.
      ValueTypeDef (Located (ASTAnnotation ast))
    | -- | type <name> = <type>
      TypeAlias (Type (ASTQual ast))

_declarationBodyExpressionLens ::
    (ASTPattern ast ~ ASTPattern ast2, ASTAnnotation ast ~ ASTAnnotation ast2, (ASTQual ast ~ ASTQual ast2)) =>
    Traversal (DeclarationBody' ast) (DeclarationBody' ast2) (ASTExpr ast) (ASTExpr ast2)
_declarationBodyExpressionLens f (Value e ps t) = fmap (\e' -> Value e' ps t) (f e)
_declarationBodyExpressionLens _ (ValueTypeDef t) = pure (ValueTypeDef t)
_declarationBodyExpressionLens _ (TypeAlias t) = pure (TypeAlias t)

newtype Import ast = Import (Located (Import' ast))

data Import' ast = Import'
    { _import'Importing :: ASTLocate ast ModuleName
    , _import'As :: Maybe (ASTLocate ast ModuleName)
    , _import'Qualified :: Bool
    , _import'Exposing :: Exposing ast
    }

data Exposing ast
    = ExposingAll
    | ExposingSome [Exposition ast]

data Exposition ast
    = ExposedValue (FullASTQual ast VarName) -- exposing foo
    | ExposedOp (FullASTQual ast OpName) -- exposing (+)
    | ExposedType (FullASTQual ast TypeName) -- exposing Foo
    | ExposedTypeAndAllConstructors (FullASTQual ast TypeName) -- exposing Foo(..)




makeLenses ''Exposing
makeLenses ''DeclarationBody
makePrisms ''DeclarationBody


makeLenses ''Declaration'

makeFields ''Module'
makePrisms ''Module
makeFields ''Import'
makePrisms ''Import
makePrisms ''Declaration
makeFields ''Declaration'

instance (a ~ [Import ast], HasImports (Module' ast) a) => HasImports (Module ast) a where
    imports = _Module . _Unlocate . imports

instance (a ~ [Declaration ast], HasDeclarations (Module' ast) a) => HasDeclarations (Module ast) a where
    declarations = _Module . _Unlocate . declarations

instance (a ~ ASTLocate ast ModuleName, HasName (Module' ast) a) => HasName (Module ast) a where
    name = _Module . _Unlocate . name

instance (a ~ Exposing  ast, HasExposing (Module' ast) a) => HasExposing (Module ast) a where
    exposing = _Module . _Unlocate . exposing

instance (a ~ ASTLocate ast ModuleName, HasImporting (Import' ast) a) => HasImporting (Import ast) a where
    importing = _Import . _Unlocate . importing

instance (a ~ Maybe (ASTLocate ast ModuleName), HasAs (Import' ast) a) => HasAs (Import ast) a where
    as = _Import . _Unlocate . as

instance (a ~ Bool, HasQualified (Import' ast) a) => HasQualified (Import ast) a where
    qualified = _Import . _Unlocate . qualified

instance (a ~ Exposing ast, HasExposing (Import' ast) a) => HasExposing (Import ast) a where
    exposing = _Import . _Unlocate . exposing



deriving instance (Show (FullASTQual ast VarName), Show (FullASTQual ast OpName), Show (FullASTQual ast TypeName)) => Show (Exposition ast)
deriving instance (Eq (FullASTQual ast VarName), Eq (FullASTQual ast OpName), Eq (FullASTQual ast TypeName)) => Eq (Exposition ast)
deriving instance (Ord (FullASTQual ast VarName), Ord (FullASTQual ast OpName), Ord (FullASTQual ast TypeName)) => Ord (Exposition ast)

deriving instance (Show (Exposition ast)) => Show (Exposing ast)
deriving instance (Eq (Exposition ast)) => Eq (Exposing ast)
deriving instance (Ord (Exposition ast)) => Ord (Exposing ast)

deriving instance (Show (FullASTQual ast ModuleName), Show (ASTLocate ast ModuleName), Show (Exposing ast)) => Show (Import' ast)
deriving instance (Eq (FullASTQual ast ModuleName), Eq (ASTLocate ast ModuleName), Eq (Exposing ast)) => Eq (Import' ast)
deriving instance (Ord (FullASTQual ast ModuleName), Ord (ASTLocate ast ModuleName), Ord (Exposing ast)) => Ord (Import' ast)

deriving instance Show (Import' ast) => Show (Import ast)
deriving instance Eq (Import' ast) => Eq (Import ast)
deriving instance Ord (Import' ast) => Ord (Import ast)

deriving instance ModConstraints Show ast => Show (Module ast)
deriving instance ModConstraints Show ast => Show (Module' ast)
deriving instance ModConstraints Show ast => Show (Declaration ast)
deriving instance ModConstraints Show ast => Show (Declaration' ast)
deriving instance ModConstraints Show ast => Show (DeclarationBody ast)
deriving instance ModConstraints Show ast => Show (DeclarationBody' ast)
deriving instance ModConstraints Eq ast => Eq (Module ast)
deriving instance ModConstraints Eq ast => Eq (Module' ast)
deriving instance ModConstraints Eq ast => Eq (Declaration ast)
deriving instance ModConstraints Eq ast => Eq (Declaration' ast)
deriving instance ModConstraints Eq ast => Eq (DeclarationBody ast)
deriving instance ModConstraints Eq ast => Eq (DeclarationBody' ast)

type NameConstraints :: (Kind.Type -> Constraint) -> (Kind.Type -> Kind.Type) -> Constraint
type NameConstraints c qual = (c (qual VarName), c (qual TypeName), c (qual OpName))

type ModConstraints :: (Kind.Type -> Constraint) -> Kind.Type -> Constraint
type ModConstraints c ast =
    ( c Name
    , c (ASTExpr ast)
    , c (ASTPattern ast)
    , c (ASTAnnotation ast)
    , c (Type (ASTQual ast))
    , c (FullASTQual ast VarName)
    , c (FullASTQual ast TypeName)
    , c (FullASTQual ast OpName)
    , c (FullASTQual ast Name)
    , c (FullASTQual ast ModuleName)
    , c (ASTLocate ast ModuleName)
    )