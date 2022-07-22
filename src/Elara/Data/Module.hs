module Elara.Data.Module where

import Data.Map qualified as M
import Elara.Data.Name hiding (_moduleName)
import Elara.Data.Type (Type)

data Module expr pattern annotation qualified = Module
  { _name :: ModuleName,
    _imports :: [Import],
    _exposing :: Exposing,
    _declarations :: M.Map Name (Declaration expr pattern annotation qualified)
  }
  deriving (Show, Eq)

data Declaration expr pattern annotation qualified = Declaration
  { module_ :: ModuleName,
    name :: Name,
    body :: DeclarationBody expr pattern annotation qualified
  }
  deriving (Show, Eq)

data DeclarationBody expr pattern annotation qualified
  = Value
      { expression :: expr,
        -- | The patterns used in things like let f x = ...
        patterns :: [pattern],
        typeAnnotation :: Maybe annotation
      }
  | -- | Used for def <name> : <type>
    ValueTypeDef annotation
  | TypeAlias (TypeAliasDeclaration qualified)
  deriving (Show, Eq)

declarationPatterns :: Declaration expr pattern annotation qualified -> [pattern]
declarationPatterns dec = case dec.body of
  v@(Value {}) -> v.patterns
  _ -> []

data TypeAliasDeclaration qualified = TypeAliasDeclaration
  { parameters :: [Name],
    definition :: Type qualified
  }
  deriving (Show, Eq)

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