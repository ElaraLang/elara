{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.VarRef where

import Control.Lens (view)
import Data.Data (Data)
import Elara.AST.Name (HasName (name), Name, Qualified, ToName (toName))
import Elara.AST.Region (IgnoreLocation (..), Located)
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.Data.Pretty (Pretty (pretty))
import Elara.Data.Unique
import Elara.Data.Unwrap (Unwrap (unwrap))

data VarRef' c n
    = Global (c (Qualified n))
    | Local (c (Unique n))
    deriving (Functor, Typeable)

type VarRef n = VarRef' Located n

type IgnoreLocVarRef n = VarRef' IgnoreLocation n

type UnlocatedVarRef n = VarRef' Identity n

varRefVal :: VarRef n -> Located n
varRefVal (Global n) = fmap (view name) n
varRefVal (Local n) = fmap (view uniqueVal) n

mkLocal :: (ToName n) => Located (Unique n) -> VarRef Name
mkLocal n = Local (toName <<$>> n)

mkLocal' :: (ToName n) => Located (Unique n) -> IgnoreLocVarRef Name
mkLocal' n = Local (toName <<$>> IgnoreLocation n)

mkGlobal :: (ToName n) => Located (Qualified n) -> VarRef Name
mkGlobal n = Global (toName <<$>> n)

mkGlobal' :: (ToName n) => Located (Qualified n) -> IgnoreLocVarRef Name
mkGlobal' n = Global (toName <<$>> IgnoreLocation n)

withName :: (ToName n) => VarRef n -> VarRef Name
withName (Global n) = Global (toName <<$>> n)
withName (Local n) = Local (toName <<$>> n)

withName' :: (ToName n) => VarRef n -> IgnoreLocVarRef Name
withName' (Global n) = Global (toName <<$>> IgnoreLocation n)
withName' (Local n) = Local (toName <<$>> IgnoreLocation n)

instance (Unwrap c, Pretty n) => Pretty (VarRef' c n) where
    pretty (Global n) = pretty (unwrap n)
    pretty (Local n) = pretty (unwrap n)

instance StripLocation (VarRef a) (UnlocatedVarRef a) where
    stripLocation v = case v of
        Global q -> Global (Identity $ stripLocation q)
        Local u -> Local (Identity $ stripLocation u)

deriving instance (Show (c (Qualified n)), Show (c (Unique n))) => Show (VarRef' c n)
deriving instance (Eq (c (Qualified n)), Eq (c (Unique n))) => Eq (VarRef' c n)
deriving instance (Ord (c (Qualified n)), Ord (c (Unique n))) => Ord (VarRef' c n)
deriving instance (Typeable c, Typeable n, Data (c (Qualified n)), Data (c (Unique n))) => Data (VarRef' c n)
