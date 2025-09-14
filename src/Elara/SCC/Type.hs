module Elara.SCC.Type where

import Data.Graph
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Elara.AST.Name
import Elara.Data.Pretty

-- Rely on Prelude/Relude re-exports for NonEmpty, sort

type BinderSet = HS.HashSet (Qualified VarName)

data ReachableSubgraph = ReachableSubgraph
    { root :: Qualified VarName
    , nodes :: BinderSet
    , edges :: HM.HashMap (Qualified VarName) BinderSet
    }
    deriving (Generic)

instance Pretty ReachableSubgraph

-- Content-based, stable identifier for an SCC: canonical sorted unique member list
newtype SCCKey = SCCKey {members :: Set (Qualified VarName)}
    deriving (Eq, Ord, Show, Generic)

instance Hashable SCCKey
sccKeyToSCC :: SCCKey -> SCC (Qualified VarName)
sccKeyToSCC (SCCKey ms) =
    case toList ms of
        [] -> error "sccKeyToSCC: empty SCCKey"
        [v] -> AcyclicSCC v
        vs -> CyclicSCC vs

sccKeyFromSCC :: SCC (Qualified VarName) -> SCCKey
sccKeyFromSCC (AcyclicSCC v) = mkSCCKey (v :| [])
sccKeyFromSCC (CyclicSCC vs) = mkSCCKey (fromList vs)

-- Build a canonical key from a non-empty set/list of members
mkSCCKey :: NonEmpty (Qualified VarName) -> SCCKey
mkSCCKey (x :| xs) = SCCKey (fromList (x : xs))
