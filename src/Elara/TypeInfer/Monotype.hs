{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | This module stores the `Monotype` type representing monomorphic types and
   utilites for operating on `Monotype`s
-}
module Elara.TypeInfer.Monotype (
    -- * Types
    Monotype (..),
    Scalar (..),
    Record (..),
    RemainingFields (..),
    Union (..),
    RemainingAlternatives (..),
)
where

import Data.Aeson (ToJSON)
import Data.Data (Data)
import Elara.Data.Pretty (Pretty (..))
import Elara.Data.Unique
import Elara.TypeInfer.Existential (Existential)
import Elara.TypeInfer.Unique

{- $setup
  >>> import qualified Elara.TypeInfer.Monotype as Monotype
  >>> import Elara.Data.Pretty
  >>> import qualified Elara.TypeInfer.Domain as Domain
-}

{- | A monomorphic type

   This is same type as `Grace.Type.Type`, except without the
   `Grace.Type.Forall` and `Grace.Type.Exists` constructors
-}
data Monotype
    = VariableType UniqueTyVar
    | UnsolvedType (Existential Monotype)
    | Function Monotype Monotype
    | Optional Monotype
    | List Monotype
    | Record Record
    | Union Union
    | Custom Text [Monotype]
    | Scalar Scalar
    deriving stock (Eq, Generic, Show, Ord)

instance ToJSON Monotype

-- | A scalar type
data Scalar
    = -- | Boolean type
      --
      -- >>> pretty Bool
      -- Bool
      Bool
    | -- | Real number type
      --
      -- >>> pretty Real
      -- Real
      Real
    | -- | Integer number type
      --
      -- >>> pretty Integer
      -- Integer
      Integer
    | -- | JSON type
      --
      -- >>> pretty JSON
      -- JSON
      JSON
    | -- | Natural number type
      --
      -- >>> pretty Natural
      -- Natural
      Natural
    | -- | Text type
      --
      -- >>> pretty Text
      -- Text
      Text
    | -- | Char type
      --
      -- >>> pretty Char
      -- Char
      Char
    | -- | Unit type
      --
      -- >>> pretty Unit
      -- Unit
      Unit
    deriving stock (Eq, Generic, Show, Data, Ord)

instance Pretty Scalar where
    pretty Bool = "Bool"
    pretty Real = "Real"
    pretty JSON = "JSON"
    pretty Natural = "Natural"
    pretty Integer = "Integer"
    pretty Text = "Text"
    pretty Unit = "Unit"
    pretty Char = "Char"

instance ToJSON Scalar

-- | A monomorphic record type
data Record = Fields [(UniqueTyVar, Monotype)] RemainingFields
    deriving stock (Eq, Generic, Show, Ord)

instance ToJSON Record

instance ToJSON RemainingFields

-- | This represents whether or not the record type is open or closed
data RemainingFields
    = -- | The record type is closed, meaning that all fields are known
      EmptyFields
    | -- | The record type is open, meaning that some fields are known and there
      --   is an unsolved fields variable that is a placeholder for other fields
      --   that may or may not be present
      UnsolvedFields (Existential Record)
    | -- | Same as `UnsolvedFields`, except that the user has given the fields
      --   variable an explicit name in the source code
      VariableFields UniqueTyVar
    deriving stock (Eq, Generic, Show, Data, Ord)

-- | A monomorphic union type
data Union = Alternatives [(UniqueTyVar, Monotype)] RemainingAlternatives
    deriving stock (Eq, Generic, Show, Ord)

instance ToJSON Union

-- | This represents whether or not the union type is open or closed
data RemainingAlternatives
    = -- | The union type is closed, meaning that all alternatives are known
      EmptyAlternatives
    | -- | The union type is open, meaning that some alternatives are known and
      --   there is an unsolved alternatives variable that is a placeholder for
      --   other alternatives that may or may not be present
      UnsolvedAlternatives (Existential Union)
    | -- | Same as `UnsolvedAlternatives`, except that the user has given the
      --   alternatives variable an explicit name in the source code
      VariableAlternatives UniqueTyVar
    deriving stock (Eq, Generic, Show, Data, Ord)

instance ToJSON RemainingAlternatives
