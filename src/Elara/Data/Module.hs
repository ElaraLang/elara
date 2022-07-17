module Elara.Data.Module where

import Data.Map qualified as M
import Elara.Data.Name
import Elara.Data.Type (Type)

data Module expr annotation qualified = Module
  { _imports :: M.Map ModuleName Import,
    _exposing :: Exposing,
    _name :: ModuleName,
    _declarations :: M.Map Name (Declaration expr annotation qualified)
  }
  deriving (Show)

data Declaration expr annotation qualified = Declaration
  { module_ :: ModuleName,
    name :: Name,
    body :: DeclarationBody expr annotation qualified
  }
  deriving (Show)

data DeclarationBody expr annotation qualified
  = Value
      { expression :: expr,
        typeAnnotation :: Maybe annotation
      }
  | TypeAlias (TypeAliasDeclaration qualified)
  deriving (Show)

data TypeAliasDeclaration qualified = TypeAliasDeclaration
  { parameters :: [Name],
    definition :: Type qualified
  }
  deriving (Show)

data Import = Import
  { _moduleName :: ModuleName,
    _as :: Maybe ModuleName,
    _qualified :: Bool,
    _exposing :: Exposing
  }
  deriving (Show)

data Exposing
  = ExposingAll
  | ExposingSome [Exposition]
  deriving (Show)

data Exposition
  = ExposedValue Name -- exposing foo
  | ExposedType Name -- exposing Foo
  | ExposedTypeAndAllConstructors Name -- exposing Foo(..)
  deriving (Show)
