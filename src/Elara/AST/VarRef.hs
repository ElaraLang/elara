{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.VarRef where

import Data.Aeson (ToJSON (..))
import Data.Data (Data)
import Elara.AST.Name (HasName (name), Name, Qualified, ToName (toName))
import Elara.AST.Region (IgnoreLocation (..), Located (..))
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.Data.Pretty (Pretty (pretty))
import Elara.Data.Unique
import Elara.Data.Unwrap (Unwrap (unwrap))

type data VarRefKind = LocatedVarRefKind | UnlocatedVarRefKind | IgnoreLocVarRefKind

type family VarRefImpl (c :: VarRefKind) a where
    VarRefImpl LocatedVarRefKind a = Located a
    VarRefImpl IgnoreLocVarRefKind a = IgnoreLocation a
    VarRefImpl UnlocatedVarRefKind a = a

class MapVarRefImpl (c :: VarRefKind) where
    mapVarRefImpl :: (a -> b) -> VarRefImpl c a -> VarRefImpl c b

instance MapVarRefImpl LocatedVarRefKind where
    mapVarRefImpl = fmap

instance MapVarRefImpl IgnoreLocVarRefKind where
    mapVarRefImpl = fmap

instance MapVarRefImpl UnlocatedVarRefKind where
    mapVarRefImpl f = f

data VarRef' (c :: VarRefKind) n
    = Global (VarRefImpl c (Qualified n))
    | Local (VarRefImpl c (Unique n))
    deriving (Typeable, Generic)

instance MapVarRefImpl c => Functor (VarRef' c) where
    fmap :: forall a b. (a -> b) -> VarRef' c a -> VarRef' c b
    fmap f (Global n) = Global $ mapVarRefImpl @c @(Qualified a) @(Qualified b) (fmap f) n
    fmap f (Local n) = Local (mapVarRefImpl @c @(Unique a) @(Unique b) (fmap f) n)

type VarRef n = VarRef' LocatedVarRefKind n

type IgnoreLocVarRef n = VarRef' IgnoreLocVarRefKind n

type UnlocatedVarRef n = VarRef' UnlocatedVarRefKind n

varRefVal :: forall c n. _ => VarRef' c n -> VarRefImpl c n
varRefVal (Global n) = mapVarRefImpl @c @(Qualified n) @n (view name) n
varRefVal (Local n) = mapVarRefImpl @c @(Unique n) @n (view uniqueVal) n

varRefVal' :: Traversal (VarRef n) (VarRef n') n n'
varRefVal' = traversalVL $ \f ->
    \case
        (Global n) -> let n' = traverse (traverse f) n in Global <$> n'
        (Local n) -> let n' = traverse (traverse f) n in Local <$> n'

ignoreLocation :: VarRef n -> IgnoreLocVarRef n
ignoreLocation (Global n) = Global (IgnoreLocation n)
ignoreLocation (Local n) = Local (IgnoreLocation n)

withName :: ToName n => VarRef n -> VarRef Name
withName (Global n) = Global (toName <<$>> n)
withName (Local n) = Local (toName <<$>> n)

withName' :: ToName n => VarRef n -> IgnoreLocVarRef Name
withName' (Global n) = Global (toName <<$>> IgnoreLocation n)
withName' (Local n) = Local (toName <<$>> IgnoreLocation n)

instance (Pretty n, Pretty (VarRefImpl c (Qualified n)), Pretty (VarRefImpl c (Unique n))) => Pretty (VarRef' c n) where
    pretty (Global n) = pretty n
    pretty (Local n) = pretty n

instance StripLocation (Located (VarRef a)) (VarRef a) where
    stripLocation = unwrap

instance StripLocation (Located (VarRef a)) (UnlocatedVarRef a) where
    stripLocation = unwrap . fmap stripLocation

instance StripLocation (VarRef a) (UnlocatedVarRef a) where
    stripLocation v = case v of
        Global q -> Global (stripLocation q)
        Local u -> Local (stripLocation u)

deriving instance (Show (VarRefImpl c (Qualified n)), Show (VarRefImpl c (Unique n))) => Show (VarRef' c n)

deriving instance (Eq (VarRefImpl c (Qualified n)), Eq (VarRefImpl c (Unique n))) => Eq (VarRef' c n)

deriving instance (Ord (VarRefImpl c (Qualified n)), Ord (VarRefImpl c (Unique n))) => Ord (VarRef' c n)

deriving instance (Typeable c, Typeable n, Data (VarRefImpl c (Qualified n)), Data (VarRefImpl c (Unique n))) => Data (VarRef' c n)

instance (Generic (VarRef' c n), ToJSON (VarRefImpl c (Unique n)), ToJSON (VarRefImpl c (Qualified n))) => ToJSON (VarRef' c n) where
    toJSON (Global n) = toJSON n
    toJSON (Local n) = toJSON n

instance (Hashable (VarRefImpl c (Qualified n)), Hashable (VarRefImpl c (Unique n))) => Hashable (VarRef' c n)
