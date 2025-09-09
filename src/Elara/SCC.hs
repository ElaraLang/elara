{-# LANGUAGE ScopedTypeVariables #-}

module Elara.SCC where

import Data.Generics.Product (HasField' (field'))
import Data.Generics.Wrapped (_Unwrapped)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Effectful
import Elara.AST.Generic.Common
import Elara.AST.Generic.Types
import Elara.AST.Name
import Elara.AST.Region (Located (..), unlocated)
import Elara.AST.Shunted
import Elara.AST.VarRef
import Elara.Error (runErrorOrReport)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Elara.SCC.Type
import Elara.Shunt.Error (ShuntError)
import Optics
import Rock qualified

runFreeVarsQuery ::
    Qualified VarName ->
    Eff
        (ConsQueryEffects '[Rock.Rock Elara.Query.Query])
        (HashSet (Qualified VarName))
runFreeVarsQuery name = do
    declaration <- runErrorOrReport @ShuntError $ Rock.fetch (Elara.Query.ShuntedDeclarationByName (NVarName <$> name))

    let body = declaration ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated

    let x = case body of
            Value _ e NoFieldValue _ _ ->
                patternDependencies e
                    <> valueDependencies e
            _ -> mempty

    pure x

runReachableSubgraphQuery :: Qualified VarName -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) ReachableSubgraph
runReachableSubgraphQuery name = do
    let moduleOf = qualifier
    let rootMod = moduleOf name

    let go :: HS.HashSet (Qualified VarName) -> HM.HashMap (Qualified VarName) BinderSet -> [Qualified VarName] -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) (BinderSet, HM.HashMap (Qualified VarName) BinderSet)
        go visited edges [] = pure (visited, edges)
        go visited edges (b : bs) = do
            depsAll <- Rock.fetch (Elara.Query.FreeVarsOf b)
            -- keep same-module deps only for SCC building
            let depsSame = HS.filter ((== rootMod) . moduleOf) depsAll
            let visited' = HS.insert b (HS.union visited depsSame)
            let edges' = HM.insertWith HS.union b depsSame edges
            let toVisit = filter (\q -> not (HS.member q visited')) (HS.toList depsSame)
            go visited' edges' (toVisit ++ bs)

    (nodes, edges) <- go HS.empty HM.empty [name]
    pure ReachableSubgraph{root = name, nodes, edges}

-- Build topologically ordered SCCs from a reachable subgraph
buildSCCs :: ReachableSubgraph -> [SCC (Qualified VarName)]
buildSCCs ReachableSubgraph{nodes, edges} =
    let triples =
            [ (n, n, filter (`HS.member` nodes) (maybe [] HS.toList (HM.lookup n edges)))
            | n <- HS.toList nodes
            ]
     in stronglyConnComp triples

-- Find the SCC that contains the root binder
sccContainingRoot :: ReachableSubgraph -> SCC (Qualified VarName)
sccContainingRoot g@ReachableSubgraph{root} =
    case find containsRoot (buildSCCs g) of
        Just s -> s
        Nothing -> AcyclicSCC root
  where
    containsRoot = \case
        AcyclicSCC v -> v == root
        CyclicSCC vs -> root `elem` vs

concatHashSetOf :: forall k is a1 a2 b. (Is k A_Fold, Hashable b) => Optic' k is a1 a2 -> (a2 -> HashSet b) -> a1 -> HashSet b
concatHashSetOf l f a =
    let folded = toListOf l a -- we do this so we don't need a2 to be hashable
     in foldMap f folded

valueDependencies :: ShuntedExpr -> HashSet (Qualified VarName)
valueDependencies =
    concatHashSetOf (cosmosOn (_Unwrapped % _1 % unlocated)) names
  where
    names :: ShuntedExpr' -> HashSet (Qualified VarName)
    names (Var (Located _ (Global e))) = one (e ^. unlocated)
    names _ = mempty

patternDependencies :: ShuntedExpr -> HashSet (Qualified VarName)
patternDependencies =
    foldOf (gplate % to patternDependencies')
  where
    patternDependencies' :: ShuntedPattern -> HashSet (Qualified VarName)
    patternDependencies' = concatHashSetOf (cosmosOnOf (_Unwrapped % _1 % unlocated) gplate) names
    names :: ShuntedPattern' -> HashSet (Qualified VarName)
    -- i think this will always be empty but just in case
    names _ = mempty

typeDependencies :: ShuntedType -> HashSet (Qualified Name)
typeDependencies =
    concatHashSetOf (cosmosOnOf (_Unwrapped % _1 % unlocated) gplate) names
  where
    names :: ShuntedType' -> HashSet (Qualified Name)
    names (UserDefinedType (Located _ e)) = one (NTypeName <$> e)
    names _ = mempty
