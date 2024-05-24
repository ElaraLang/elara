module Elara.Emit.Method.Descriptor where

import Elara.Data.Pretty
import Elara.Data.Unique
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Type

data NamedMethodDescriptor = NamedMethodDescriptor [(Unique Text, FieldType)] ReturnDescriptor
    deriving (Show)
instance Pretty NamedMethodDescriptor
toMethodDescriptor :: NamedMethodDescriptor -> MethodDescriptor
toMethodDescriptor (NamedMethodDescriptor args ret) = MethodDescriptor (snd <$> args) ret
