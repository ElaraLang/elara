-- | Types for working with strongly connected components (SCCs)
module Elara.SCC.Type (sccKeyFromSCC, sccKeyToSCC, SCCKey, ReachableSubgraph (..)) where

import Data.Graph
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Elara.AST.Name
import Elara.Data.Pretty

-- | A set of binders represented as a hash set of qualified variable names
type BinderSet = HS.HashSet (Qualified VarName)

-- | A reachable subgraph from a given root binder
data ReachableSubgraph = ReachableSubgraph
    { root :: Qualified VarName
    -- ^ The root binder from which this subgraph is reachable
    , nodes :: BinderSet
    -- ^ The set of binders in this subgraph
    , edges :: HM.HashMap (Qualified VarName) BinderSet
    -- ^ The edges between binders in this subgraph
    }
    deriving (Generic)

instance Pretty ReachableSubgraph

-- | Content-based, stable identifier for an SCC: canonical sorted unique member list
newtype SCCKey = SCCKey {members :: Set (Qualified VarName)}
    deriving (Eq, Ord, Show, Generic)

instance Hashable SCCKey

-- | Convert between 'SCCKey' and 'SCC (Qualified VarName)'
sccKeyToSCC :: SCCKey -> SCC (Qualified VarName)
sccKeyToSCC (SCCKey ms) =
    case toList ms of
        [] -> error "sccKeyToSCC: empty SCCKey"
        [v] -> AcyclicSCC v
        vs -> CyclicSCC vs

-- | Convert an 'SCC (Qualified VarName)' to its canonical 'SCCKey'
sccKeyFromSCC :: SCC (Qualified VarName) -> SCCKey
sccKeyFromSCC (AcyclicSCC v) = mkSCCKey (v :| [])
sccKeyFromSCC (CyclicSCC vs) = mkSCCKey (fromList vs)

-- | Build a canonical key from a non-empty set/list of members
mkSCCKey :: NonEmpty (Qualified VarName) -> SCCKey
mkSCCKey (x :| xs) = SCCKey (fromList (x : xs))
