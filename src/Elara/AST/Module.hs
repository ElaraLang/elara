{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Elara.AST.Module where

import Control.Lens (makeClassy, makeFields, makeLenses, makePrisms)
import Data.Kind qualified as Kind (Type)
import Elara.AST.Name (ModuleName, Name, OpName, TypeName, VarName)
import Elara.AST.Select (ASTDeclaration, ASTExpr, ASTLocate, ASTPattern, ASTType, Frontend, FullASTQual, HasModuleName (moduleName, unlocatedModuleName), HasName (name), RUnlocate (..), UnlocatedFrontend)
import Elara.AST.StripLocation
import Unsafe.Coerce (unsafeCoerce)

newtype Module ast = Module (ASTLocate ast (Module' ast))

data Module' ast = Module'
    { _module'Name :: ASTLocate ast ModuleName
    , _module'Exposing :: Exposing ast
    , _module'Imports :: [Import ast]
    , _module'Declarations :: [ASTDeclaration ast]
    }

newtype Import ast = Import (ASTLocate ast (Import' ast))

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

{- | Safe coercion between 'Exposition' types
 Since the ASTX type families aren't injective, we can't use 'coerce' :(
-}
coerceExposition ::
    ( FullASTQual ast1 VarName ~ FullASTQual ast2 VarName
    , FullASTQual ast1 OpName ~ FullASTQual ast2 OpName
    , FullASTQual ast1 TypeName ~ FullASTQual ast2 TypeName
    ) =>
    Exposition ast1 ->
    Exposition ast2
coerceExposition = unsafeCoerce

coerceExposing ::
    ( FullASTQual ast1 VarName ~ FullASTQual ast2 VarName
    , FullASTQual ast1 OpName ~ FullASTQual ast2 OpName
    , FullASTQual ast1 TypeName ~ FullASTQual ast2 TypeName
    ) =>
    Exposing ast1 ->
    Exposing ast2
coerceExposing = unsafeCoerce

coerceImport ::
    ( ASTLocate ast1 ModuleName ~ ASTLocate ast2 ModuleName
    , FullASTQual ast1 VarName ~ FullASTQual ast2 VarName
    , FullASTQual ast1 OpName ~ FullASTQual ast2 OpName
    , FullASTQual ast1 TypeName ~ FullASTQual ast2 TypeName
    ) =>
    Import ast1 ->
    Import ast2
coerceImport = unsafeCoerce

-- Vile lens and deriving boilerplate

makeFields ''Module'
makeLenses ''Module'
makePrisms ''Module
makePrisms ''Import
makeClassy ''Import'
makeFields ''Import'

instance {-# OVERLAPPING #-} (RUnlocate ast, a ~ [Import ast], HasImports (Module' ast) a) => HasImports (Module ast) a where
    imports f mo@(Module m) =
        let m' = rUnlocate' @ast m :: Module' ast
         in fmap (const mo) (f (m'._module'Imports :: [Import ast]))

instance {-# OVERLAPPING #-} (RUnlocate ast, a ~ [ASTDeclaration ast], HasDeclarations (Module' ast) a) => HasDeclarations (Module ast) a where
    declarations f mo@(Module m) =
        let m' = rUnlocate' @ast m :: Module' ast
         in fmap (const mo) (f (m'._module'Declarations))

instance (RUnlocate ast, a ~ ASTLocate ast ModuleName, HasName (Module' ast) a) => HasName (Module ast) a where
    name f mo@(Module m) =
        let m' = rUnlocate' @ast m :: Module' ast
         in fmap (const mo) (f (m'._module'Name))

instance (RUnlocate ast, a ~ Exposing ast, HasExposing (Module' ast) a) => HasExposing (Module ast) a where
    exposing f mo@(Module m) =
        let m' = rUnlocate' @ast m :: Module' ast
         in fmap (const mo) (f (m'._module'Exposing))

instance (RUnlocate ast, a ~ ASTLocate ast ModuleName, HasImporting (Import' ast) a) => HasImporting (Import ast) a where
    importing f im@(Import i) =
        let i' = rUnlocate' @ast i :: Import' ast
         in fmap (const im) (f (i'._import'Importing))

instance (RUnlocate ast, a ~ Maybe (ASTLocate ast ModuleName), HasAs (Import' ast) a) => HasAs (Import ast) a where
    as f im@(Import i) =
        let i' = rUnlocate' @ast i :: Import' ast
         in fmap (const im) (f (i'._import'As))

instance (RUnlocate ast, a ~ Bool, HasQualified (Import' ast) a) => HasQualified (Import ast) a where
    qualified f im@(Import i) =
        let i' = rUnlocate' @ast i :: Import' ast
         in fmap (const im) (f (i'._import'Qualified))

instance (RUnlocate ast, a ~ Exposing ast, HasExposing (Import' ast) a) => HasExposing (Import ast) a where
    exposing f im@(Import i) =
        let i' = rUnlocate' @ast i :: Import' ast
         in fmap (const im) (f (i'._import'Exposing))

deriving instance (Show (FullASTQual ast VarName), Show (FullASTQual ast OpName), Show (FullASTQual ast TypeName)) => Show (Exposition ast)
deriving instance (Eq (FullASTQual ast VarName), Eq (FullASTQual ast OpName), Eq (FullASTQual ast TypeName)) => Eq (Exposition ast)
deriving instance (Ord (FullASTQual ast VarName), Ord (FullASTQual ast OpName), Ord (FullASTQual ast TypeName)) => Ord (Exposition ast)

deriving instance (Show (Exposition ast)) => Show (Exposing ast)
deriving instance (Eq (Exposition ast)) => Eq (Exposing ast)
deriving instance (Ord (Exposition ast)) => Ord (Exposing ast)

deriving instance (Show (FullASTQual ast ModuleName), Show (ASTLocate ast ModuleName), Show (Exposing ast)) => Show (Import' ast)
deriving instance (Eq (FullASTQual ast ModuleName), Eq (ASTLocate ast ModuleName), Eq (Exposing ast)) => Eq (Import' ast)
deriving instance (Ord (FullASTQual ast ModuleName), Ord (ASTLocate ast ModuleName), Ord (Exposing ast)) => Ord (Import' ast)

deriving instance Show (ASTLocate ast (Import' ast)) => Show (Import ast)
deriving instance Eq (ASTLocate ast (Import' ast)) => Eq (Import ast)
deriving instance Ord (ASTLocate ast (Import' ast)) => Ord (Import ast)

deriving instance (ModConstraints Show ast) => Show (Module ast)
deriving instance ModConstraints Show ast => Show (Module' ast)
deriving instance ModConstraints Eq ast => Eq (Module ast)
deriving instance ModConstraints Eq ast => Eq (Module' ast)

type NameConstraints :: (Kind.Type -> Constraint) -> (Kind.Type -> Kind.Type) -> Constraint
type NameConstraints c qual = (c (qual VarName), c (qual TypeName), c (qual OpName))

type ModConstraints :: (Kind.Type -> Constraint) -> Kind.Type -> Constraint
type ModConstraints c ast =
    ( c Name
    , c (ASTExpr ast)
    , c (ASTPattern ast)
    , c (ASTType ast)
    , c (ASTDeclaration ast)
    , c (FullASTQual ast VarName)
    , c (FullASTQual ast TypeName)
    , c (FullASTQual ast OpName)
    , c (FullASTQual ast Name)
    , c (ASTLocate ast ModuleName)
    , c (ASTLocate ast (Module' ast))
    , c (ASTLocate ast (Import' ast))
    , c (ASTLocate ast (Exposition ast))
    , c (ASTLocate ast (Exposing ast))
    )

instance (RUnlocate ast) => HasModuleName (Module ast) ast where
    moduleName = _Module @ast @ast . (rUnlocated' @ast) . module'Name @ast
    unlocatedModuleName = moduleName @(Module ast) @ast . rUnlocated' @ast

instance StripLocation (Module Frontend) (Module UnlocatedFrontend) where
    stripLocation (Module m) = Module (stripLocation (stripLocation m :: Module' Frontend))

instance StripLocation (Module' Frontend) (Module' UnlocatedFrontend) where
    stripLocation (Module' n e i d) = Module' (stripLocation n) (stripLocation e) (stripLocation i) (stripLocation d)

instance StripLocation (Exposing Frontend) (Exposing UnlocatedFrontend) where
    stripLocation ExposingAll = ExposingAll
    stripLocation (ExposingSome e) = ExposingSome (stripLocation e)

instance StripLocation (Exposition Frontend) (Exposition UnlocatedFrontend) where
    stripLocation (ExposedValue n) = ExposedValue (stripLocation n)
    stripLocation (ExposedType tn) = ExposedType (stripLocation tn)
    stripLocation (ExposedTypeAndAllConstructors tn) = ExposedTypeAndAllConstructors (stripLocation tn)
    stripLocation (ExposedOp o) = ExposedOp (stripLocation o)

instance StripLocation (Import Frontend) (Import UnlocatedFrontend) where
    stripLocation (Import m) = Import (stripLocation (stripLocation m :: Import' Frontend))

instance StripLocation (Import' Frontend) (Import' UnlocatedFrontend) where
    stripLocation (Import' i a q e) = Import' (stripLocation i) (stripLocation a) q (stripLocation e)
