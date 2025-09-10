module Elara.Core.LiftClosures where

import Data.Set qualified as Set
import Effectful
import Effectful.Writer.Static.Local
import Elara.AST.Name (ModuleName)
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.ANF as ANF
import Elara.Core.Analysis
import Elara.Core.Generic (Bind (..), traverseBind)
import Elara.Core.Generic qualified as G
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Elara.Core.ToANF (fromANF)
import Elara.Data.Pretty
import Elara.Data.Unique.Effect (UniqueGen, makeUnique)
import Elara.Logging (StructuredDebug, debug, traceFn)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Rock qualified
import Prelude hiding (Alt)

type LiftClosures r = (UniqueGen :> r, Writer [(Core.Var, CExpr Core.Var)] :> r, StructuredDebug :> r)

runLiftClosures :: (UniqueGen :> r, StructuredDebug :> r) => CoreModule (G.Bind Core.Var ANF.Expr) -> Eff r (CoreModule (G.Bind Core.Var ANF.Expr))
runLiftClosures (CoreModule m decls) = do
    (decls', closures) <- runWriter $ for decls $ \decl -> do
        case decl of
            CoreValue v -> do
                v' <- traverseBind pure liftClosures v
                pure $ CoreValue v'
            CoreType t -> pure $ CoreType t
    let closures' = map (\(v, e) -> CoreValue $ NonRecursive (v, ANF.CExpr e)) closures
    pure $ CoreModule m (decls' <> closures')

runGetClosureLiftedModuleQuery ::
    ModuleName ->
    Eff
        (ConsQueryEffects '[Rock.Rock Elara.Query.Query, UniqueGen, StructuredDebug])
        (CoreModule (G.Bind Core.Var ANF.Expr))
runGetClosureLiftedModuleQuery mn = do
    coreModule <- Rock.fetch (Elara.Query.GetANFCoreModule mn)
    runLiftClosures coreModule

{- | Lift closures in a Core expression. This turns all closures into top-level functions.
For example:
@
let add5 x =
   let f y = x + y
   in f 5
@
becomes:
@
let add5_f x y = x + y
let add5 x = add5_f x 5
@
-}
liftClosures :: LiftClosures r => Expr Core.Var -> Eff r (Expr Core.Var)
liftClosures = liftClosures' Set.empty

closedVariables e = filerLocal $ freeCoreVars e
  where
    filerLocal =
        Set.filter
            ( \case
                Core.Id (Local _) _ _ -> True
                _ -> False
            )

liftClosures' :: LiftClosures r => Set.Set Core.Var -> ANF.Expr Core.Var -> Eff r (ANF.Expr Core.Var)
liftClosures' env (ANF.Let (NonRecursive (v, e)) e') = do
    debug $ "Lifting closure: " <> pretty v
    debug $ "closed: " <> pretty (closedVariables e)
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

liftClosuresC :: LiftClosures r => CExpr Core.Var -> Eff r (CExpr Core.Var)
liftClosuresC = liftClosuresC' Set.empty

liftClosuresC' :: LiftClosures r => Set.Set Core.Var -> ANF.CExpr Core.Var -> Eff r (CExpr Core.Var)
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

liftClosuresA' :: LiftClosures r => Set.Set Core.Var -> ANF.AExpr Core.Var -> Eff r (AExpr Core.Var)
liftClosuresA' _ other@(ANF.Var{}) = pure other
liftClosuresA' _ (ANF.Lit l) = pure $ Lit l
liftClosuresA' env (ANF.Lam v e) = do
    -- let freeVars = freeCoreVars e `Set.difference` env
    if True
        then do
            e' <- liftClosures' env e
            pure $ Lam v e'
        else do
            v' <- makeUnique "closure"
            lambdaType <- case v of
                Core.Id _ t _ -> do
                    guessedType <- traceFn guesstimateExprType (fromANF e)
                    pure $ t `Core.FuncTy` guessedType
                Core.TyVar _ -> error "liftClosuresA': TyVar"
            let id = Core.Id (Local v') lambdaType Nothing
            tell [(id, ANF.AExpr $ Lam v e)]
            pure $ Var id
liftClosuresA' env (ANF.TyApp e t) = do
    e' <- liftClosuresA' env e
    pure $ TyApp e' t
liftClosuresA' env (ANF.TyLam t e) = do
    e' <- liftClosuresA' env e
    pure $ TyLam t e'
