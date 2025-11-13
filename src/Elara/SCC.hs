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
import Elara.AST.Select
import Elara.AST.Shunted
import Elara.AST.VarRef
import Elara.Error (runErrorOrReport)
import Elara.Query (QueryType (QueryRequiredDeclarationByName), SupportsQuery)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Elara.SCC.Type
import Elara.Shunt ()
import Elara.Shunt.Error (ShuntError)
import Optics
import Rock qualified

runFreeVarsQuery ::
    SupportsQuery QueryRequiredDeclarationByName Shunted =>
    Qualified VarName ->
    Eff
        (ConsQueryEffects '[Rock.Rock Elara.Query.Query])
        (HashSet (Qualified VarName))
runFreeVarsQuery name = do
    declaration <- runErrorOrReport @ShuntError $ Rock.fetch (Elara.Query.RequiredDeclarationByName @Shunted (NVarName <$> name))

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

    let go ::
            HS.HashSet (Qualified VarName) -> -- discovered (seen) nodes
            HM.HashMap (Qualified VarName) (HS.HashSet (Qualified VarName)) ->
            [Qualified VarName] -> -- worklist
            Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) (HS.HashSet (Qualified VarName), HM.HashMap (Qualified VarName) (HS.HashSet (Qualified VarName)))
        go seen edges [] = pure (seen, edges)
        go seen edges (b : bs) = do
            depsAll <- Rock.fetch (Elara.Query.FreeVarsOf b)
            let depsSame = HS.filter ((== rootMod) . moduleOf) depsAll
            -- only enqueue deps we haven’t seen yet
            let newDeps = HS.difference depsSame seen
            let seen' = HS.insert b (HS.union seen newDeps)
            let edges' = HM.insertWith HS.union b depsSame edges
            let bs' = HS.toList newDeps ++ bs
            go seen' edges' bs'

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
