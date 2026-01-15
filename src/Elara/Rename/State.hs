module Elara.Rename.State where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.Data.Unique

{- | The state used during renaming. Keeps track of all in-scope known names.
Specifically, it maps the "unqualified" name ot all known references to that name.
-}
data RenameState = RenameState
    { varNames :: Map VarName (NonEmpty (VarRef VarName))
    -- ^ All the variable names in scope
    , typeNames :: Map TypeName (NonEmpty (VarRef TypeName))
    -- ^ All the type names in scope
    , typeVars :: Map LowerAlphaName (Unique LowerAlphaName)
    -- ^ All the type variables in scope
    }
    deriving (Show, Generic)

-- | Insert a value into a map of non-empty lists, merging with existing entries.
insertMerging ::
    (Ord k, Eq a) => k -> a -> Map k (NonEmpty a) -> Map k (NonEmpty a)
insertMerging k x = Map.insertWith ((NonEmpty.nub .) . (<>)) k (one x)

instance Semigroup RenameState where
    RenameState v1 t1 tv1 <> RenameState v2 t2 tv2 =
        RenameState (v1 <> v2) (t1 <> t2) (tv1 <> tv2)

instance Monoid RenameState where
    mempty = RenameState mempty mempty mempty
