{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Data.Module where

import Control.Lens (Plated)
import Control.Lens.Plated (plate)
import Control.Lens.TH (makeFields, makeLenses)
import Data.Data (Data)
import Data.Map qualified as M
import Data.Multimap qualified as Mu
import Data.Unique (Unique)
import Elara.Data.Name hiding (_moduleName)
import Elara.Data.Type (ConcreteType)
import Elara.Data.Uniqueness

data Module expr pattern annotation qualified uniqueness = Module
  { _moduleName :: ModuleName,
    _moduleImports :: [Import],
    _moduleExposing :: Exposing,
    _moduleDeclarations :: Structure uniqueness Name (Declaration expr pattern annotation qualified)
  }

-- most disgusting line of code i've ever written
deriving instance
  (Show (Structure uniqueness Name (Declaration expr pattern annotation qualified))) =>
  Show (Module expr pattern annotation qualified uniqueness)

deriving instance
  (Eq (Structure uniqueness Name (Declaration expr pattern annotation qualified))) =>
  Eq (Module expr pattern annotation qualified uniqueness)

data Declaration expr pattern annotation qualified = Declaration
  { _declarationModule_ :: ModuleName,
    _declarationName :: Name,
    _declarationBody :: DeclarationBody expr pattern annotation qualified
  }
  deriving (Show, Eq, Data)

instance (Data expr, Data pattern, Data annotation, Data qualified) => Plated (Declaration expr pattern annotation qualified)

data DeclarationBody expr pattern annotation qualified
  = Value
      { _declarationBodyExpression :: expr,
        -- | The patterns used in things like let f x = ...
        _declarationBodyPatterns :: [pattern],
        _declarationBodyTypeAnnotation :: Maybe annotation
      }
  | -- | Used for def <name> : <type>
    ValueTypeDef annotation
  | TypeAlias (TypeAliasDeclaration qualified)
  deriving (Show, Eq, Data)

mapExpr :: (expr -> expr') -> DeclarationBody expr pattern annotation qualified -> DeclarationBody expr' pattern annotation qualified
mapExpr f (Value e p a) = Value (f e) p a
mapExpr _ (ValueTypeDef a) = ValueTypeDef a
mapExpr _ (TypeAlias d) = TypeAlias d

declarationPatterns :: Declaration expr pattern annotation qualified -> [pattern]
declarationPatterns dec = case dec._declarationBody of
  v@(Value {}) -> v._declarationBodyPatterns
  _ -> []

data TypeAliasDeclaration qualified = TypeAliasDeclaration
  { _parameters :: [Name],
    _definition :: ConcreteType qualified
  }
  deriving (Show, Eq, Data)

data Import = Import
  { _importImporting :: ModuleName,
    _importAs :: Maybe ModuleName,
    _importQualified :: Bool,
    _importExposing :: Exposing
  }
  deriving (Ord, Eq, Show)

data Exposing
  = ExposingAll
  | ExposingSome [Exposition]
  deriving (Ord, Eq, Show)

data Exposition
  = ExposedValue Name -- exposing foo
  | ExposedType Name -- exposing Foo
  | ExposedTypeAndAllConstructors Name -- exposing Foo(..)
  deriving (Ord, Eq, Show)

makeLenses ''Exposing
makeLenses ''DeclarationBody
makeLenses ''Module
makeLenses ''Declaration

makeFields ''Module
makeFields ''Import
makeFields ''Declaration