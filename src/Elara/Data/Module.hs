module Elara.Data.Module where

import Elara.Data.Name
import Elara.Data.Pattern (Pattern)
import Elara.Data.Type (Type)

data Module expr annotation qualified = Module
  { _name :: ModuleName,
    _imports :: [Import],
    _exposing :: Exposing,
    _declarations :: [Declaration expr annotation qualified]
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
        -- | The patterns used in things like let f x = ...
        patterns :: [Pattern],
        typeAnnotation :: Maybe annotation
      }
  | -- | Used for def <name> : <type>
    ValueTypeDef annotation
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
