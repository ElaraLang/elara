module Elara.AST.Generic.Common where

import Data.Data
import Elara.Data.Pretty

data DataConCantHappen deriving (Generic, Data, Show, Eq, Ord)

instance Pretty DataConCantHappen where
    pretty :: HasCallStack => DataConCantHappen -> Doc AnsiStyle
    pretty _ = error "DataConCantHappen"

dataConCantHappen :: DataConCantHappen -> a
dataConCantHappen x = case x of {}

data NoFieldValue = NoFieldValue
    deriving (Generic, Data, Show, Eq, Ord)

instance Pretty NoFieldValue where
    pretty :: NoFieldValue -> Doc AnsiStyle
    pretty _ = "NoFieldValue"
