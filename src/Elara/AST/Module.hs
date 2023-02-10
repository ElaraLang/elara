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
import Elara.AST.Name (ModuleName, Name, TypeName, VarName)
import Elara.AST.Select (ASTAnnotation, ASTExpr, ASTPattern, ASTQual)
import Elara.Data.Type
import Prelude hiding (Type)
import Prelude qualified as Kind

data Module ast = Module
    { _moduleName :: ModuleName
    , _moduleExposing :: Exposing (ASTQual ast)
    , _moduleImports :: [Import (ASTQual ast)]
    , _moduleDeclarations :: [Declaration ast]
    }

moduleDeclarations :: (ASTQual ast ~ ASTQual ast2) => Lens (Module ast) (Module ast2) [Declaration ast] [Declaration ast2]
moduleDeclarations f (Module n e i d) = fmap (Module n e i) (f d)

type ModConstraints :: (Kind.Type -> Constraint) -> Kind.Type -> Constraint
type ModConstraints c ast =
    ( c (Name (ASTQual ast))
    , c (ASTExpr ast)
    , c (ASTPattern ast)
    , c (ASTAnnotation ast)
    , c (Type (ASTQual ast))
    , c ((ASTQual ast) VarName)
    , c ((ASTQual ast) TypeName)
    )

deriving instance ModConstraints Show ast => Show (Module ast)
deriving instance ModConstraints Eq ast => Eq (Module ast)

data Declaration ast = Declaration
    { _declarationModule' :: ModuleName
    , _declarationName :: Name (ASTQual ast)
    , _declarationBody :: DeclarationBody ast
    }

_declarationBodyLens :: (ASTQual ast ~ ASTQual ast2) => Lens (Declaration ast) (Declaration ast2) (DeclarationBody ast) (DeclarationBody ast2)
_declarationBodyLens f (Declaration m n b) = fmap (Declaration m n) (f b)

deriving instance ModConstraints Show ast => Show (Declaration ast)
deriving instance ModConstraints Eq ast => Eq (Declaration ast)

data DeclarationBody ast
    = -- | let <p> = <e>
      Value
        { _declarationBodyExpression :: ASTExpr ast
        , _declarationBodyPatterns :: [ASTPattern ast]
        -- ^ The patterns used in things like let f x = ...
        , _declarationBodyTypeAnnotation :: ASTAnnotation ast
        }
    | -- | def <name> : <type>.
      ValueTypeDef (ASTAnnotation ast)
    | -- | type <name> = <type>
      TypeAlias (Type (ASTQual ast))

_declarationBodyExpressionLens :: (ASTPattern ast ~ ASTPattern ast2, ASTAnnotation ast ~ ASTAnnotation ast2, (ASTQual ast ~ ASTQual ast2)) => Traversal (DeclarationBody ast) (DeclarationBody ast2) (ASTExpr ast) (ASTExpr ast2)
_declarationBodyExpressionLens f (Value e ps t) = fmap (\e' -> Value e' ps t) (f e)
_declarationBodyExpressionLens _ (ValueTypeDef t) = pure (ValueTypeDef t)
_declarationBodyExpressionLens _ (TypeAlias t) = pure (TypeAlias t)

deriving instance ModConstraints Show ast => Show (DeclarationBody ast)
deriving instance ModConstraints Eq ast => Eq (DeclarationBody ast)

type NameConstraints :: (Kind.Type -> Constraint) -> (Kind.Type -> Kind.Type) -> Constraint
type NameConstraints c qual = (c (qual VarName), c (qual TypeName))
data Import qual = Import
    { _importImporting :: ModuleName
    , _importAs :: Maybe ModuleName
    , _importQualified :: Bool
    , _importExposing :: Exposing qual
    }

deriving instance (NameConstraints Show qual) => Show (Import qual)
deriving instance (NameConstraints Eq qual) => Eq (Import qual)
deriving instance (NameConstraints Ord qual) => Ord (Import qual)

data Exposing qual
    = ExposingAll
    | ExposingSome [Exposition qual]

deriving instance (NameConstraints Show qual) => Show (Exposing qual)
deriving instance (NameConstraints Eq qual) => Eq (Exposing qual)
deriving instance (NameConstraints Ord qual) => Ord (Exposing qual)

data Exposition qual
    = ExposedValue (qual VarName) -- exposing foo
    | ExposedType (qual TypeName) -- exposing Foo
    | ExposedTypeAndAllConstructors (qual TypeName) -- exposing Foo(..)

deriving instance (NameConstraints Show qual) => Show (Exposition qual)
deriving instance (NameConstraints Eq qual) => Eq (Exposition qual)
deriving instance (NameConstraints Ord qual) => Ord (Exposition qual)

makeLenses ''Exposing
makeLenses ''DeclarationBody
makePrisms ''DeclarationBody

-- makeLenses ''Module
makeLenses ''Declaration

makeFields ''Module
makeFields ''Import
makeFields ''Declaration