module Elara.AST.Generic.Common where

import Data.Data
import Elara.Data.Pretty

data DataConCantHappen deriving (Generic, Data, Show)

instance Pretty DataConCantHappen where
  pretty :: (HasCallStack) => DataConCantHappen -> Doc AnsiStyle
  pretty _ = error "DataConCantHappen"

instance Eq DataConCantHappen where
  (==) :: (HasCallStack) => DataConCantHappen -> DataConCantHappen -> Bool
  (==) = error "DataConCantHappen"

dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

data NoFieldValue = NoFieldValue
  deriving (Generic, Data, Show, Eq)

instance Pretty NoFieldValue where
  pretty :: (HasCallStack) => NoFieldValue -> Doc AnsiStyle
  pretty _ = error "This instance should never be used"
