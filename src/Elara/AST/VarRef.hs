{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.VarRef where

import Control.Lens (Traversal, view)
import Data.Aeson (ToJSON (..))
import Data.Data (Data)
import Elara.AST.Name (HasName (name), Name, Qualified, ToName (toName))
import Elara.AST.Region (IgnoreLocation (..), Located (..))
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.Data.Pretty (Pretty (pretty))
import Elara.Data.Unique
import Elara.Data.Unwrap (Unwrap (unwrap))

data VarRef' c n
    = Global (c (Qualified n))
    | Local (c (Unique n))
    deriving (Functor, Typeable, Generic)

type VarRef n = VarRef' Located n

type IgnoreLocVarRef n = VarRef' IgnoreLocation n

type UnlocatedVarRef n = VarRef' Identity n

pattern Global' :: Qualified n -> VarRef' Identity n
pattern Global' n = Global (Identity n)

pattern Local' :: Unique n -> VarRef' Identity n
pattern Local' n = Local (Identity n)

{-# COMPLETE Global', Local' #-}

varRefVal :: Functor c => VarRef' c n -> c n
varRefVal (Global n) = fmap (view name) n
varRefVal (Local n) = fmap (view uniqueVal) n

varRefVal' :: Traversal (VarRef n) (VarRef n') n n'
varRefVal' f (Global n) = let n' = traverse (traverse f) n in Global <$> n'
varRefVal' f (Local n) = let n' = traverse (traverse f) n in Local <$> n'

ignoreLocation :: VarRef n -> IgnoreLocVarRef n
ignoreLocation (Global n) = Global (IgnoreLocation n)
ignoreLocation (Local n) = Local (IgnoreLocation n)

mkLocal :: ToName n => Located (Unique n) -> VarRef Name
mkLocal n = Local (toName <<$>> n)

mkLocal' :: ToName n => Located (Unique n) -> IgnoreLocVarRef Name
mkLocal' n = Local (toName <<$>> IgnoreLocation n)

mkGlobal :: ToName n => Located (Qualified n) -> VarRef Name
mkGlobal n = Global (toName <<$>> n)

mkGlobal' :: ToName n => Located (Qualified n) -> IgnoreLocVarRef Name
mkGlobal' n = Global (toName <<$>> IgnoreLocation n)

mkGlobalUnlocated :: ToName n => Qualified n -> UnlocatedVarRef Name
mkGlobalUnlocated n = Global (Identity (toName <$> n))

mkLocalUnlocated :: ToName n => Unique n -> UnlocatedVarRef Name
mkLocalUnlocated n = Local (Identity (toName <$> n))

withName :: ToName n => VarRef n -> VarRef Name
withName (Global n) = Global (toName <<$>> n)
withName (Local n) = Local (toName <<$>> n)

withName' :: ToName n => VarRef n -> IgnoreLocVarRef Name
withName' (Global n) = Global (toName <<$>> IgnoreLocation n)
withName' (Local n) = Local (toName <<$>> IgnoreLocation n)

instance (Unwrap c, Pretty n) => Pretty (VarRef' c n) where
    pretty (Global n) = pretty (unwrap n)
    pretty (Local n) = pretty (unwrap n)

instance StripLocation (Located (VarRef a)) (VarRef a) where
    stripLocation = unwrap

instance StripLocation (Located (VarRef a)) (UnlocatedVarRef a) where
    stripLocation = unwrap . fmap stripLocation

instance StripLocation (VarRef a) (UnlocatedVarRef a) where
    stripLocation v = case v of
        Global q -> Global (Identity $ stripLocation q)
        Local u -> Local (Identity $ stripLocation u)

deriving instance (Show (c (Qualified n)), Show (c (Unique n))) => Show (VarRef' c n)

deriving instance (Eq (c (Qualified n)), Eq (c (Unique n))) => Eq (VarRef' c n)

deriving instance (Ord (c (Qualified n)), Ord (c (Unique n))) => Ord (VarRef' c n)

deriving instance (Typeable c, Typeable n, Data (c (Qualified n)), Data (c (Unique n))) => Data (VarRef' c n)

instance (Generic (VarRef' c n), ToJSON (c (Unique n)), ToJSON (c (Qualified n))) => ToJSON (VarRef' c n) where
    toJSON (Global n) = toJSON n
    toJSON (Local n) = toJSON n

instance (Hashable (c (Qualified n)), Hashable (c (Unique n))) => Hashable (VarRef' c n)
