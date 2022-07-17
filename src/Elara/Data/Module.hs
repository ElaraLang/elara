module Elara.Data.Module where

import Data.Map qualified as M
import Elara.Data.Name
import Elara.Data.Type (Type)

data Module expr annotation qualified = Module
  { imports :: M.Map ModuleName Import,
    exposing :: Exposing,
    name :: ModuleName,
    declarations :: M.Map Name (Declaration expr annotation qualified)
  }

data Declaration expr annotation qualified = Declaration
  { module_ :: ModuleName,
    name :: Name,
    body :: DeclarationBody expr annotation qualified
  }

data DeclarationBody expr annotation qualified
  = Value
      { expression :: expr,
        typeAnnotation :: Maybe annotation
      }
  | TypeAlias (TypeAliasDeclaration qualified)

data TypeAliasDeclaration qualified = TypeAliasDeclaration
  { parameters :: [Name],
    definition :: Type qualified
  }

data Import = Import
  { moduleName :: ModuleName,
    as :: Maybe ModuleName,
    qualified :: Bool,
    exposing :: Maybe Exposing
  }

data Exposing
  = ExposingAll
  | ExposingSome [Exposition]

data Exposition
  = ExposedValue Name -- exposing foo
  | ExposedType Name -- exposing Foo
  | ExposedTypeAndAllConstructors Name -- exposing Foo(..)
