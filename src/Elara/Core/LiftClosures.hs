module Elara.Core.LiftClosures where

import Data.Set qualified as Set
import Effectful
import Effectful.Writer.Static.Local
import Elara.AST.Name (ModuleName, Qualified (..))
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.ANF as ANF
import Elara.Core.Analysis
import Elara.Core.Generic (Bind (..), traverseBind)
import Elara.Core.Generic qualified as G
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Elara.Core.ToANF (fromANF)
import Elara.Data.Pretty
import Elara.Data.Unique (uniqueToText)
import Elara.Data.Unique.Effect (UniqueGen, makeUnique)
import Elara.Logging (StructuredDebug, debug, traceFn)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Rock qualified
import Prelude hiding (Alt)

type LiftClosures r = (HasCallStack, UniqueGen :> r, Writer [(Core.Var, CExpr Core.Var)] :> r, StructuredDebug :> r)

runLiftClosures :: (HasCallStack, UniqueGen :> r, StructuredDebug :> r) => CoreModule (G.Bind Core.Var ANF.Expr) -> Eff r (CoreModule (G.Bind Core.Var ANF.Expr))
runLiftClosures (CoreModule m decls) = do
    (decls', closures) <- runWriter $ for decls $ \decl -> do
        case decl of
            CoreValue v -> do
                v' <- traverseBind pure (liftClosures m) v
                pure $ CoreValue v'
            CoreType t -> pure $ CoreType t
    let closures' = map (\(v, e) -> CoreValue $ NonRecursive (v, ANF.CExpr e)) closures
    pure $ CoreModule m (closures' <> decls')

runGetClosureLiftedModuleQuery ::
    HasCallStack =>
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
liftClosures :: LiftClosures r => ModuleName -> Expr Core.Var -> Eff r (Expr Core.Var)
liftClosures m = liftClosures' m Set.empty

closedVariables :: FreeCoreVars ast => ast Core.Var -> Set Core.Var
closedVariables e = filerLocal $ freeCoreVars e
  where
    filerLocal =
        Set.filter
            ( \case
                Core.Id (Local _) _ _ -> True
                _ -> False
            )

liftClosures' :: LiftClosures r => ModuleName -> Set.Set Core.Var -> ANF.Expr Core.Var -> Eff r (ANF.Expr Core.Var)
-- Let binding where RHS is a lambda
liftClosures' m env (ANF.Let (NonRecursive (v, ANF.AExpr (ANF.Lam lamV lamBody))) rest) =
    handleLambda
        m
        env
        lamV
        lamBody
        ( \body' -> do
            rest' <- liftClosures' m (Set.insert v env) rest
            pure $ Let (NonRecursive (v, ANF.AExpr (ANF.Lam lamV body'))) rest' -- keep as is if no capture
        )
        ( \liftedId captures ->
            mkClosureChain m v liftedId captures rest -- start lifting
        )
-- Lambda expression
liftClosures' m env (ANF.CExpr (ANF.AExpr (ANF.Lam lamV lamBody))) =
    handleLambda
        m
        env
        lamV
        lamBody
        (pure . ANF.CExpr . ANF.AExpr . ANF.Lam lamV) -- if no capture, keep as is
        ( \liftedId captures -> do
            vName <- makeUnique "res"
            let resultType = case liftedId of
                    Core.Id _ t _ -> t
                    Core.TyVar _ -> error "TyVar in lifted lambda"
            let v = Core.Id (Local vName) resultType Nothing
            mkClosureChain m v liftedId captures (ANF.CExpr (ANF.AExpr (ANF.Var v))) -- otherwise start lifting
        )
liftClosures' m env (ANF.Let (NonRecursive (v, e)) e') = do
    debug $ "Lifting closure: " <> pretty v
    debug $ "closed: " <> pretty (closedVariables e)
    e' <- liftClosures' m (Set.insert v env) e'
    pure $ Let (NonRecursive (v, e)) e'
liftClosures' m env (ANF.Let (Recursive bs) e) = do
    let env' = env <> Set.fromList (map fst bs)
    e' <- liftClosures' m env' e
    bs' <- for bs $ \(v, e) -> do
        e' <- liftClosuresC' m env' e
        pure (v, e')
    pure $ Let (Recursive bs') e'
liftClosures' m env (ANF.CExpr e) = ANF.CExpr <$> liftClosuresC' m env e

liftClosuresC :: LiftClosures r => ModuleName -> CExpr Core.Var -> Eff r (CExpr Core.Var)
liftClosuresC m = liftClosuresC' m Set.empty

liftClosuresC' ::
    HasCallStack =>
    LiftClosures r =>
    ModuleName ->
    Set.Set Core.Var ->
    ANF.CExpr Core.Var ->
    Eff r (CExpr Core.Var)
liftClosuresC' m env (ANF.App f x) = do
    f' <- liftClosuresA' m env f
    x' <- liftClosuresA' m env x
    pure $ App f' x'
liftClosuresC' m env (ANF.AExpr e) = AExpr <$> liftClosuresA' m env e
liftClosuresC' m env (ANF.Match e v alts) = do
    e' <- liftClosuresA' m env e
    alts' <- for alts $ \(con, bs, e) -> do
        e' <- liftClosures' m (env <> Set.fromList bs) e
        pure (con, bs, e')
    pure $ Match e' v alts'

liftClosuresA' :: HasCallStack => LiftClosures r => ModuleName -> Set.Set Core.Var -> ANF.AExpr Core.Var -> Eff r (AExpr Core.Var)
liftClosuresA' _ _ other@(ANF.Var{}) = pure other
liftClosuresA' _ _ (ANF.Lit l) = pure $ Lit l
liftClosuresA' m env (ANF.Lam v body) = do
    -- We assume capturing lambdas are caught by liftClosures' before reaching here.
    -- This handles only truly pure lambdas nested inside other structures.
    body' <- liftClosures' m (Set.insert v env) body
    pure $ Lam v body'
liftClosuresA' m env (ANF.TyApp e t) = do
    e' <- liftClosuresA' m env e
    pure $ TyApp e' t
liftClosuresA' m env (ANF.TyLam t e) = do
    e' <- liftClosuresA' m env e
    pure $ TyLam t e'

isLocal :: Core.Var -> Bool
isLocal (Core.Id (Local _) _ _) = True
isLocal _ = False

handleLambda ::
    LiftClosures r =>
    ModuleName ->
    Set.Set Core.Var ->
    Core.Var ->
    Expr Core.Var ->
    -- | what to do if nothing is captured
    (Expr Core.Var -> Eff r (Expr Core.Var)) ->
    -- | what to do if something is captured
    (Core.Var -> [Core.Var] -> Eff r (Expr Core.Var)) ->
    Eff r (Expr Core.Var)
handleLambda m env lamV lamBody onNoCapture onCapture = do
    let fullFreeVars = freeCoreVars lamBody
    let captures = Set.toList $ Set.filter isLocal fullFreeVars `Set.difference` Set.insert lamV env
    if null captures
        then do
            body' <- liftClosures' m (Set.insert lamV env) lamBody
            onNoCapture body'
        else do
            (liftedId, _) <- createTopLevelLambda m env lamV lamBody captures
            onCapture liftedId captures

createTopLevelLambda :: LiftClosures r => ModuleName -> Set.Set Core.Var -> Core.Var -> Expr Core.Var -> [Core.Var] -> Eff r (Core.Var, Core.Type)
createTopLevelLambda modName env lamV lamBody captures = do
    v' <- makeUnique "lifted"

    -- determine lifted function type
    -- TODO: this is probably not good
    originalType <- case lamV of
        Core.Id _ t _ -> do
            ret <- traceFn guesstimateExprType (fromANF lamBody)
            pure $ t `Core.FuncTy` ret
        _ -> error "TyVar in lambda"

    let liftedType = foldr (\(Core.Id _ t _) acc -> t `Core.FuncTy` acc) originalType captures
    let qualifiedName = Qualified (uniqueToText identity v') modName
    let liftedId = Core.Id (Global qualifiedName) liftedType Nothing

    body' <-
        liftClosures'
            modName
            (Set.insert lamV (env <> Set.fromList captures))
            lamBody

    -- Wrap body: \c1 -> \c2 -> ... \arg -> body
    let mkLam v b = ANF.CExpr $ ANF.AExpr $ ANF.Lam v b
    let newBody = foldr mkLam (mkLam lamV body') captures

    case newBody of
        ANF.CExpr e -> tell [(liftedId, e)]
        _ -> error "Body construction failed"

    pure (liftedId, liftedType)

{- | helper function to build closure chain
It turns an n-ary application into a sequence of binary applications (since ANF only supports binary applications)

For example, given:
  liftedFn :: A -> B -> C -> Ret
  captures = [c1, c2]
  finalVar :: C -> Ret

It will produce the ANF equivalent of
  @let finalVar = liftedFn c1 c2@

i.e. in proper ANF: @
  let partial1 = liftedFn c1
  let finalVar = partial1 c2@
-}
mkClosureChain :: (HasCallStack, LiftClosures r) => ModuleName -> Core.Var -> Core.Var -> [Core.Var] -> Expr Core.Var -> Eff r (Expr Core.Var)
mkClosureChain modName finalVar liftedFn captures rest = do
    rest' <- liftClosures' modName (Set.fromList captures) rest

    let go prevVar [] = pure $ Let (NonRecursive (finalVar, ANF.AExpr (ANF.Var prevVar))) rest'
        go prevVar (c : cs) = do
            -- Extract return type from function type for the intermediate variable
            let nextType = case prevVar of
                    Core.Id _ (Core.FuncTy _ ret) _ -> ret
                    _ -> error "mkClosureChain: Expected FuncTy"

            if null cs
                then pure $ Let (NonRecursive (finalVar, ANF.App (ANF.Var prevVar) (ANF.Var c))) rest'
                else do
                    freshVar <- makeUniqueVar "partial" nextType
                    inner <- go freshVar cs
                    pure $ Let (NonRecursive (freshVar, ANF.App (ANF.Var prevVar) (ANF.Var c))) inner

    go liftedFn captures

makeUniqueVar :: UniqueGen :> r => Text -> Core.Type -> Eff r Core.Var
makeUniqueVar name t = do
    u <- makeUnique name
    pure $ Core.Id (Local u) t Nothing
