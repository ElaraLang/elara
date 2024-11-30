module Elara.Core.LiftClosures where

import Data.Set qualified as Set
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.ANF as ANF
import Elara.Core.Analysis
import Elara.Core.Generic (Bind (..), traverseBind)
import Elara.Core.Generic qualified as G
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Elara.Core.ToANF (fromANF)
import Elara.Data.Unique (UniqueGen, makeUnique)
import Polysemy
import Polysemy.Writer (Writer, runWriter, tell)
import Prelude hiding (Alt)

type LiftClosures r = Members '[UniqueGen, Writer [(Core.Var, CExpr Core.Var)]] r

runLiftClosures :: Member UniqueGen r => CoreModule (G.Bind Core.Var ANF.Expr) -> Sem r (CoreModule (G.Bind Core.Var ANF.Expr))
runLiftClosures (CoreModule m decls) = do
    (closures, decls') <- runWriter $ for decls $ \decl -> do
        case decl of
            CoreValue v -> do
                v' <- traverseBind pure liftClosures v
                pure $ CoreValue v'
            CoreType t -> pure $ CoreType t
    let closures' = map (\(v, e) -> CoreValue $ NonRecursive (v, ANF.CExpr e)) closures
    pure $ CoreModule m (decls' <> closures')

{- | Lift closures in a Core expression. This turns all closures into top-level functions.
For example:
@
let f = \foo -> foo (\bar -> bar foo)
@
becomes:
@
let f = \foo -> foo ((\foo -> \bar -> bar foo) foo)
@
and then:
@
let f' = \foo -> \bar -> bar foo
let f = \foo -> foo f'
@
-}
liftClosures :: LiftClosures r => Expr Core.Var -> Sem r (Expr Core.Var)
liftClosures = liftClosures' Set.empty

liftClosures' :: LiftClosures r => Set.Set Core.Var -> ANF.Expr Core.Var -> Sem r (ANF.Expr Core.Var)
liftClosures' env (ANF.Let (NonRecursive (v, e)) e') = do
    e' <- liftClosures' (Set.insert v env) e'
    pure $ Let (NonRecursive (v, e)) e'
liftClosures' env (ANF.Let (Recursive bs) e) = do
    let env' = env <> Set.fromList (map fst bs)
    e' <- liftClosures' env' e
    bs' <- for bs $ \(v, e) -> do
        e' <- liftClosuresC' env' e
        pure (v, e')
    pure $ Let (Recursive bs') e'
liftClosures' env (ANF.CExpr e) = ANF.CExpr <$> liftClosuresC' env e

liftClosuresC :: LiftClosures r => CExpr Core.Var -> Sem r (CExpr Core.Var)
liftClosuresC = liftClosuresC' Set.empty

liftClosuresC' :: LiftClosures r => Set.Set Core.Var -> ANF.CExpr Core.Var -> Sem r (CExpr Core.Var)
liftClosuresC' env (ANF.App f x) = do
    f' <- liftClosuresA' env f
    x' <- liftClosuresA' env x
    pure $ App f' x'
liftClosuresC' env (ANF.AExpr e) = AExpr <$> liftClosuresA' env e
liftClosuresC' env (ANF.Match e v alts) = do
    e' <- liftClosuresA' env e
    alts' <- for alts $ \(con, bs, e) -> do
        e' <- liftClosures' (env <> Set.fromList bs) e
        pure (con, bs, e')
    pure $ Match e' v alts'

liftClosuresA' :: LiftClosures r => Set.Set Core.Var -> ANF.AExpr Core.Var -> Sem r (AExpr Core.Var)
liftClosuresA' _ other@(ANF.Var{}) = pure other
liftClosuresA' _ (ANF.Lit l) = pure $ Lit l
liftClosuresA' env (ANF.Lam v e) = do
    let freeVars = freeCoreVars e `Set.difference` env
    if Set.null freeVars
        then do
            e' <- liftClosures' env e
            pure $ Lam v e'
        else do
            v' <- makeUnique "closure"
            let id = Core.Id (Local' v') (exprType (fromANF e)) Nothing
            tell [(id, ANF.AExpr $ Lam v e)]
            pure $ Var id
liftClosuresA' env (ANF.TyApp e t) = do
    e' <- liftClosuresA' env e
    pure $ TyApp e' t
liftClosuresA' env (ANF.TyLam t e) = do
    e' <- liftClosuresA' env e
    pure $ TyLam t e'
