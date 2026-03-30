{-# LANGUAGE ScopedTypeVariables #-}

module Elara.SCC where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Effectful
import Elara.AST.Name
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Shunted (Shunted)
import Elara.AST.Phases.Shunted qualified as NewS
import Elara.AST.Region (Located (..), unlocated)
import Elara.AST.Types qualified as New
import Elara.AST.VarRef
import Elara.Error (runErrorOrReport)
import Elara.Query (QueryType (QueryRequiredDeclarationByName), SupportsQuery)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Elara.SCC.Type
import Elara.Shunt ()
import Elara.Shunt.Error (ShuntError)
import Optics (view)
import Rock qualified

runFreeVarsQuery ::
    SupportsQuery QueryRequiredDeclarationByName Shunted =>
    Qualified VarName ->
    Eff
        (ConsQueryEffects '[Rock.Rock Elara.Query.Query])
        (HashSet (Qualified VarName))
runFreeVarsQuery name = do
    declaration <- runErrorOrReport @ShuntError $ Rock.fetch (Elara.Query.RequiredDeclarationByName @Shunted (NVarName <$> name))

    let New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body)) = declaration

    let x = case body of
            New.ValueDeclaration _ e _ _ _ _ ->
                patternDependencies e <> valueDependencies e
            New.TypeDeclarationBody{} -> mempty
            New.DeclBodyExtension v -> absurd v

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
            -- only enqueue deps we haven't seen yet
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

-- | Collect free variable references from an expression (manual recursion replacing cosmosOf plate)
valueDependencies :: NewS.ShuntedExpr -> HashSet (Qualified VarName)
valueDependencies (New.Expr _ _ e') = case e' of
    New.EVar NoExtension (Located _ (Global (Located _ qn))) -> one qn
    New.EVar _ _ -> mempty
    New.ECon _ _ -> mempty
    New.EInt _ -> mempty
    New.EFloat _ -> mempty
    New.EString _ -> mempty
    New.EChar _ -> mempty
    New.EUnit -> mempty
    New.ELam _ _ body -> valueDependencies body
    New.EApp _ f x -> valueDependencies f <> valueDependencies x
    New.ETyApp e _ -> valueDependencies e
    New.EIf c t f -> valueDependencies c <> valueDependencies t <> valueDependencies f
    New.EMatch e cases -> valueDependencies e <> foldMap (\(_, b) -> valueDependencies b) cases
    New.ELetIn _ _ e1 e2 -> valueDependencies e1 <> valueDependencies e2
    New.ELet _ _ e1 -> valueDependencies e1
    New.EBlock exprs -> foldMap valueDependencies exprs
    New.EAnn e _ -> valueDependencies e
    New.EExtension v -> absurd v

-- | Collect pattern dependencies (currently always empty but kept for completeness)
patternDependencies :: NewS.ShuntedExpr -> HashSet (Qualified VarName)
patternDependencies _ = mempty

-- | Collect type dependencies from a shunted type
typeDependencies :: NewS.ShuntedType -> HashSet (Qualified Name)
typeDependencies (New.Type _ _ t') = case t' of
    New.TUserDefined (Located _ e) -> one (NTypeName <$> e)
    New.TFun t1 t2 -> typeDependencies t1 <> typeDependencies t2
    New.TApp t1 t2 -> typeDependencies t1 <> typeDependencies t2
    New.TList t -> typeDependencies t
    New.TRecord fields -> foldMap (typeDependencies . snd) (toList fields)
    New.TVar _ -> mempty
    New.TUnit -> mempty
    New.TExtension v -> absurd v
