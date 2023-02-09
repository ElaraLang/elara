{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module Elara.AST.Module where

import Control.Lens (Lens, makeFields, makeLenses, makePrisms)
import Control.Lens.Traversal
import Elara.AST.Name (MaybeQualified, ModuleName, Name, TypeName, VarName)
import Elara.AST.Select (ASTAnnotation, ASTExpr, ASTPattern, ASTQual)
import Elara.Data.Type
import Prelude hiding (Type)
import Prelude qualified as Kind

data Module ast = Module
    { _moduleName :: ModuleName
    , _moduleExposing :: Exposing
    , _moduleImports :: [Import]
    , _moduleDeclarations :: [Declaration ast]
    }
type ModConstraints :: (Kind.Type -> Constraint) -> Kind.Type -> Constraint
type ModConstraints c ast =
    ( c (Name (ASTQual ast))
    , c (ASTExpr ast)
    , c (ASTPattern ast)
    , c (ASTAnnotation ast)
    , c (Type (ASTQual ast))
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

data Import = Import
    { _importImporting :: ModuleName
    , _importAs :: Maybe ModuleName
    , _importQualified :: Bool
    , _importExposing :: Exposing
    }
    deriving (Ord, Eq, Show)

data Exposing
    = ExposingAll
    | ExposingSome [Exposition]
    deriving (Ord, Eq, Show)

data Exposition
    = ExposedValue (MaybeQualified VarName) -- exposing foo
    | ExposedType (MaybeQualified TypeName) -- exposing Foo
    | ExposedTypeAndAllConstructors (MaybeQualified TypeName) -- exposing Foo(..)
    deriving (Ord, Eq, Show)

makeLenses ''Exposing
makeLenses ''DeclarationBody
makePrisms ''DeclarationBody
makeLenses ''Module
makeLenses ''Declaration

makeFields ''Module
makeFields ''Import
makeFields ''Declaration