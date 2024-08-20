module Elara.Emit.Method.Descriptor where

import Elara.Data.Pretty
import Elara.Data.Unique
import JVM.Data.Abstract.Descriptor
import JVM.Data.Abstract.Type

data NamedMethodDescriptor = NamedMethodDescriptor [(Unique Text, FieldType)] ReturnDescriptor
    deriving (Show)

methodDescriptorTypes :: NamedMethodDescriptor -> [FieldType]
methodDescriptorTypes (NamedMethodDescriptor m _) = fmap snd m
instance Pretty NamedMethodDescriptor where
    pretty (NamedMethodDescriptor args ret) = pretty ret <+> tupled (prettyArg <$> args)
      where
        prettyArg (u, t) = pretty t <+> pretty u

toMethodDescriptor :: NamedMethodDescriptor -> MethodDescriptor
toMethodDescriptor (NamedMethodDescriptor args ret) = MethodDescriptor (snd <$> args) ret
