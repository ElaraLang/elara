{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Compile-time checking that query keys contain no source locations
module Elara.Query.Generics (LocationFreeIn, checkKey) where

import Data.Kind (Type)
import GHC.Generics
import GHC.TypeError
import GHC.TypeLits (Symbol)

import Elara.AST.Location (NodeLoc, TaggedLocate)
import Elara.AST.Region (Located, SourceRegion)

{- | @LocationFreeIn con field a@ holds when @a@ contains no location at any depth.
@con@ and @field@ only exist to name the offending query key in the error message.
-}
class LocationFreeIn (con :: Symbol) (field :: Nat) a

checkKey :: forall (con :: Symbol) (field :: Nat) k. LocationFreeIn con field k => ()
checkKey = ()

type LocatedKeyMsg con field t =
    'Text "Query key " ':<>: 'Text con ':<>: 'Text " (field " ':<>: 'ShowType field ':<>: 'Text ") contains a source location:"
        ':$$: 'Text "    " ':<>: 'ShowType t
        ':$$: 'Text "Keys with location information do not behave in a predictable way."
        ':$$: 'Text "Hint: for error handling, key on the unlocated value and attach locations where the error is reported."

instance Unsatisfiable (LocatedKeyMsg con field SourceRegion) => LocationFreeIn con field SourceRegion
instance Unsatisfiable (LocatedKeyMsg con field (Located a)) => LocationFreeIn con field (Located a)
instance Unsatisfiable (LocatedKeyMsg con field (TaggedLocate n loc a)) => LocationFreeIn con field (TaggedLocate n loc a)
instance Unsatisfiable (LocatedKeyMsg con field (NodeLoc n loc)) => LocationFreeIn con field (NodeLoc n loc)

instance LocationFreeIn con field Text
instance LocationFreeIn con field Char
instance LocationFreeIn con field Int
instance LocationFreeIn con field Integer
instance LocationFreeIn con field a => LocationFreeIn con field (Set a)
instance LocationFreeIn con field a => LocationFreeIn con field (HashSet a)

instance {-# OVERLAPPABLE #-} (Generic a, GLocationFreeIn con field (Rep a)) => LocationFreeIn con field a

class GLocationFreeIn (con :: Symbol) (field :: Nat) (f :: Type -> Type)

instance GLocationFreeIn con field f => GLocationFreeIn con field (M1 i m f)
instance (GLocationFreeIn con field l, GLocationFreeIn con field r) => GLocationFreeIn con field (l :*: r)
instance (GLocationFreeIn con field l, GLocationFreeIn con field r) => GLocationFreeIn con field (l :+: r)
instance GLocationFreeIn con field U1
instance GLocationFreeIn con field V1
instance LocationFreeIn con field c => GLocationFreeIn con field (K1 i c)
