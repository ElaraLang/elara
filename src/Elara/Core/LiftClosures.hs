module Elara.Core.LiftClosures (
    ClosureLiftError (..),
    runLiftClosures,
    runGetClosureLiftedModuleQuery,
    liftClosures,
) where

import Control.Monad (foldM)
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Elara.AST.Name (ModuleName, Qualified (..))
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.ANF as ANF
import Elara.Core.Analysis
import Elara.Core.Generic (Bind (..), traverseBind)
import Elara.Core.Generic qualified as G
import Elara.Core.LiftClosures.Error
import Elara.Core.LiftClosures.Util
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Elara.Core.ToANF (fromANF)
import Elara.Data.Pretty
import Elara.Data.Unique (uniqueToText, uniqueVal)
import Elara.Data.Unique.Effect (UniqueGen, makeUnique)
import Elara.Logging (StructuredDebug, debug, debugWith, traceFn)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects)
import Rock qualified
import Prelude hiding (Alt)

data LiftContext = LiftContext
    { lcModuleName :: ModuleName
    -- ^ Current module name
    , lcParentName :: Maybe Text
    -- ^ Name of parent binding, used for naming lifted functions
    , lcEnvironment :: Set.Set Core.Var
    -- ^ Currently bound variables, i.e. locals in scope
    }
    deriving (Generic)
instance Pretty LiftContext

type LiftClosures r =
    ( HasCallStack
    , UniqueGen :> r
    , Writer [(Core.Var, CExpr Core.Var)] :> r
    , StructuredDebug :> r
    , Error ClosureLiftError :> r
    , Reader LiftContext :> r
    )

-- | Get the term type or throw an error
requireTermType :: Error ClosureLiftError :> r => Core.Var -> Eff r Core.Type
requireTermType v = case varTermType v of
    Just t -> pure t
    Nothing -> throwError (TyVarInTermPosition v)

-- | Require a CExpr, throwing an error if not
requireCExpr ::
    Error ClosureLiftError :> r =>
    -- | context for error message
    Text ->
    -- | expression to check
    Expr Core.Var ->
    -- | extracted CExpr
    Eff r (CExpr Core.Var)
requireCExpr _ (ANF.CExpr e) = pure e
requireCExpr ctx _ = throwError (ExpectedCExpr ctx)

-- | Run the closure lifting pass on a Core module
runLiftClosures :: (HasCallStack, UniqueGen :> r, StructuredDebug :> r, Error ClosureLiftError :> r) => CoreModule (G.Bind Core.Var ANF.Expr) -> Eff r (CoreModule (G.Bind Core.Var ANF.Expr))
runLiftClosures (CoreModule m decls) = do
    (decls', closures) <- runWriter $ for decls $ \case
        CoreValue bind -> case bind of
            NonRecursive (v, e) -> do
                -- Use the binding name as parent for lifting its RHS
                let parentName = extractVarName v
                e' <- runReader (LiftContext m (Just parentName) Set.empty) (liftClosures' e)
                pure $ CoreValue (NonRecursive (v, e'))
            Recursive bs -> do
                -- For recursive groups, give each RHS its binder name when lifting
                bs' <- for bs $ \(v, e) -> do
                    let parentName = extractVarName v
                    e' <- runReader (LiftContext m (Just parentName) Set.empty) (liftClosures' e)
                    pure (v, e')
                pure $ CoreValue (Recursive bs')
        CoreType t -> pure $ CoreType t
    let closures' = map (\(v, e) -> CoreValue $ NonRecursive (v, ANF.CExpr e)) closures
    pure $ CoreModule m (closures' <> decls')

runGetClosureLiftedModuleQuery ::
    HasCallStack =>
    ModuleName ->
    Eff
        (ConsQueryEffects '[Rock.Rock Elara.Query.Query, UniqueGen, StructuredDebug, Error ClosureLiftError])
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
liftClosures ::
    ( HasCallStack
    , UniqueGen :> r
    , Writer [(Core.Var, CExpr Core.Var)] :> r
    , StructuredDebug :> r
    , Error ClosureLiftError :> r
    ) =>
    ModuleName -> Expr Core.Var -> Eff r (Expr Core.Var)
liftClosures m expr =
    runReader (LiftContext m Nothing Set.empty) (liftClosures' expr)

-- | Generate a name for a lifted function
mkLiftedName ::
    LiftClosures r =>
    Text ->
    Eff r Text
mkLiftedName base = do
    u <- makeUnique base
    pure (uniqueToText identity u)

-- | Actually perform closure lifting on an expression
liftClosures' ::
    LiftClosures r =>
    ANF.Expr Core.Var ->
    Eff r (ANF.Expr Core.Var)
-- Let binding where RHS is a lambda - use binding name for context
liftClosures' (ANF.Let (NonRecursive (v, ANF.AExpr (ANF.Lam lamV lamBody))) rest) = do
    let varName = extractVarName v
    local (\ctx -> ctx{lcParentName = Just varName}) $ do
        (liftedId, captures) <- createTopLevelLambda lamV lamBody
        rest' <- local (extendEnv v) (liftClosures' rest)
        mkClosureChain v liftedId captures rest'
-- Lambda expression - use generic name
liftClosures' (ANF.CExpr (ANF.AExpr (ANF.Lam lamV lamBody))) = do
    (liftedId, captures) <- createTopLevelLambda lamV lamBody
    vName <- makeUnique "res"
    resultType <- requireTermType liftedId
    let v = Core.Id (Local vName) resultType Nothing
    mkClosureChain v liftedId captures (ANF.CExpr (ANF.AExpr (ANF.Var v)))
liftClosures' (ANF.Let (NonRecursive (v, ANF.App f x)) rest)
    | isLam f || isLam x = local (withParentName (extractVarName v)) $ do
        (fWrap, f') <- extractLambda f
        (xWrap, x') <- extractLambda x
        let newExpr = fWrap $ xWrap $ Let (NonRecursive (v, ANF.App f' x')) rest
        liftClosures' newExpr
liftClosures' (ANF.CExpr (ANF.App f x))
    | isLam f || isLam x = do
        (fWrap, f') <- extractLambda f
        (xWrap, x') <- extractLambda x
        liftClosures' (fWrap $ xWrap $ ANF.CExpr (ANF.App f' x'))
-- Regular let binding
liftClosures' (ANF.Let (NonRecursive (v, value)) body) = do
    let varName = extractVarName v
    eLifted <- local (withParentName varName) (liftClosuresC' value)
    bodyLifted <- local (extendEnv v) (liftClosures' body)
    pure $ Let (NonRecursive (v, eLifted)) bodyLifted
-- Recursive bindings
liftClosures' (ANF.Let (Recursive bs) e) =
    liftRecursiveBindings bs e
-- CExpr
liftClosures' (ANF.CExpr e) = ANF.CExpr <$> liftClosuresC' e

-- | Extract variable name for better naming
extractVarName :: Core.Var -> Text
extractVarName (Core.Id (Local u) _ _) = u ^. uniqueVal
extractVarName (Core.Id (Global (Qualified n _)) _ _) = n
extractVarName _ = "anon"

-- | Choose a descriptive base name for lifted functions
chooseBaseName :: LiftContext -> Core.Var -> [Core.Var] -> Text
chooseBaseName ctx lamV captures =
    let param = extractVarName lamV
     in case lcParentName ctx of
            Just parent -> parent <> "_" <> param <> "_lambda"
            Nothing -> case captures of
                (c : _) -> extractVarName c <> "_" <> param <> "_lambda"
                [] -> param <> "_lambda"

-- | Extend environment with a new binding
extendEnv :: Core.Var -> LiftContext -> LiftContext
extendEnv v ctx = ctx{lcEnvironment = Set.insert v (lcEnvironment ctx)}

extendEnv' :: Set.Set Core.Var -> LiftContext -> LiftContext
extendEnv' vs ctx = ctx{lcEnvironment = Set.union vs (lcEnvironment ctx)}

-- | Set parent name in context
withParentName :: Text -> LiftContext -> LiftContext
withParentName name ctx = ctx{lcParentName = Just name}

-- | Compute captures for a lambda
computeLambdaCaptures :: LiftClosures r => Core.Var -> Expr Core.Var -> Eff r [Core.Var]
computeLambdaCaptures lamV lamBody =
    pure $ Set.toList $ Set.filter (\v -> v /= lamV && not (isGlobal v)) (freeCoreVars lamBody)

-- | Lift closures in a CExpr
liftClosuresC' ::
    LiftClosures r =>
    ANF.CExpr Core.Var ->
    Eff r (CExpr Core.Var)
liftClosuresC' (ANF.App f x) = do
    f' <- liftClosuresA' f
    x' <- liftClosuresA' x
    pure $ App f' x'
liftClosuresC' (ANF.AExpr e) = AExpr <$> liftClosuresA' e
liftClosuresC' (ANF.Match e v alts) = do
    e' <- liftClosuresA' e
    alts' <- for alts $ \(con, bs, e) -> do
        ctx <- ask
        let ctx' = flipfoldl' extendEnv ctx bs
        e' <- local (const ctx') (liftClosures' e)
        pure (con, bs, e')
    pure $ Match e' v alts'

-- | Lift closures in an AExpr
liftClosuresA' ::
    LiftClosures r =>
    ANF.AExpr Core.Var ->
    Eff r (AExpr Core.Var)
liftClosuresA' other@(ANF.Var{}) = pure other
liftClosuresA' (ANF.Lit l) = pure $ Lit l
liftClosuresA' (ANF.Lam v body) = do
    (liftedId, _) <- createTopLevelLambda v body
    pure $ Var liftedId
liftClosuresA' (ANF.TyApp e t) = do
    e' <- liftClosuresA' e
    pure $ TyApp e' t
liftClosuresA' (ANF.TyLam t e) = do
    e' <- liftClosuresA' e
    pure $ TyLam t e'

createTopLevelLambda ::
    LiftClosures r =>
    Core.Var ->
    Expr Core.Var ->
    Eff r (Core.Var, [Core.Var])
createTopLevelLambda lamV lamBody = do
    captures <- computeLambdaCaptures lamV lamBody
    ctx <- ask
    let baseName = chooseBaseName ctx lamV captures
    liftedName <- mkLiftedName baseName

    -- Compute types
    lamVType <- requireTermType lamV
    ret <- traceFn guesstimateExprType (fromANF lamBody)
    let originalType = lamVType `Core.FuncTy` ret

    liftedTy <- case liftedType captures originalType of
        Just t -> pure t
        Nothing -> throwError (CannotDetermineType lamV)

    let qualifiedName = Qualified liftedName (lcModuleName ctx)
    let liftedId = Core.Id (Global qualifiedName) liftedTy Nothing

    -- Lift body with extended environment
    let bodyCtx = flipfoldl' extendEnv (extendEnv lamV ctx) captures
    body' <- local (const bodyCtx) (liftClosures' lamBody)

    -- Wrap body, turning \c1 -> \c2 -> into \arg -> body
    let newBody = mkNestedLam captures (mkNestedLam [lamV] body')

    finalCExpr <- requireCExpr "createTopLevelLambda body" newBody
    tell [(liftedId, finalCExpr)]

    pure (liftedId, captures)

makeUniqueVar :: UniqueGen :> r => Text -> Core.Type -> Eff r Core.Var
makeUniqueVar name t = do
    u <- makeUnique name
    pure $ Core.Id (Local u) t Nothing

-- | Extract an expression into a let binding if it's a lambda
extractLambda ::
    LiftClosures r =>
    ANF.AExpr Core.Var ->
    Eff r (Expr Core.Var -> Expr Core.Var, ANF.AExpr Core.Var)
extractLambda (ANF.Lam v body) = do
    ctx <- ask
    let baseName = maybe "arg" (<> "_arg") (lcParentName ctx)
    freshName <- makeUnique baseName
    argType <- requireTermType v
    retType <- traceFn guesstimateExprType (fromANF body)
    let fnType = argType `Core.FuncTy` retType
    let freshId = Core.Id (Local freshName) fnType Nothing
    let wrapper = Let (NonRecursive (freshId, ANF.AExpr (ANF.Lam v body)))
    pure (wrapper, ANF.Var freshId)
extractLambda other = pure (identity, other)

{- | Build a closure application chain

Turns @liftedFn c1 c2 ... cn@
Into @let p1 = liftedFn c1 in let p2 = p1 c2 in ... let result = pN cn@
-}
mkClosureChain ::
    LiftClosures r =>
    -- | The final variable to bind
    Core.Var ->
    -- | The lifted function
    Core.Var ->
    -- | Captured variables to apply
    [Core.Var] ->
    -- | Continuation
    Expr Core.Var ->
    Eff r (Expr Core.Var)
mkClosureChain finalVar liftedFn [] rest =
    -- No captures, just bind the lifted function directly
    pure $ Let (NonRecursive (finalVar, ANF.AExpr (ANF.Var liftedFn))) rest
mkClosureChain finalVar liftedFn captures rest = go liftedFn captures
  where
    go prevVar [] =
        pure $ Let (NonRecursive (finalVar, ANF.AExpr (ANF.Var prevVar))) rest
    go prevVar [c] =
        pure $ Let (NonRecursive (finalVar, ANF.App (ANF.Var prevVar) (ANF.Var c))) rest
    go prevVar (c : cs) = do
        prevType <- requireTermType prevVar
        nextType <- case prevType of
            Core.FuncTy _ ret -> pure ret
            other -> throwError (ExpectedFunctionType prevVar other)
        freshVar <- makeUniqueVar "partial" nextType
        inner <- go freshVar cs
        pure $ Let (NonRecursive (freshVar, ANF.App (ANF.Var prevVar) (ANF.Var c))) inner

-- | Information about a single binding in a recursive group
data RecBindingInfo = RecBindingInfo
    { rbiOriginalVar :: Core.Var
    -- ^ Original local variable
    , rbiOriginalRhs :: CExpr Core.Var
    -- ^ Original RHS
    , rbiLiftedVar :: Core.Var
    -- ^ New global variable
    , rbiLiftedType :: Core.Type
    -- ^ Type of lifted function
    }

{- | Lift a recursive binding group

Recursive bindings are tricky because:
1. All bindings in the group may reference each other
2. They all share the same set of captures
3. Each lifted function needs "fixups" to reference the other lifted functions
-}
liftRecursiveBindings ::
    LiftClosures r =>
    [(Core.Var, CExpr Core.Var)] ->
    Expr Core.Var ->
    Eff r (Expr Core.Var)
liftRecursiveBindings bindings cont = do
    -- Compute shared captures for the entire group
    let boundVars = Set.fromList (map fst bindings)
    let allFreeVars = foldMap (freeCoreVars . snd) bindings
    let captures = computeGroupCaptures boundVars allFreeVars

    -- Generate global IDs for all bindings with descriptive names
    bindingInfos <- for bindings $ \(v, rhs) -> do
        let varName = extractVarName v
        liftedName <- local (withParentName varName) (mkLiftedName "rec")

        originalType <- requireTermType v
        liftedTy <- case liftedType captures originalType of
            Just t -> pure t
            Nothing -> throwError (CannotDetermineType v)

        ctx <- ask
        let qualifiedName = Qualified liftedName (lcModuleName ctx)
        let liftedVar = Core.Id (Global qualifiedName) liftedTy Nothing

        pure
            RecBindingInfo
                { rbiOriginalVar = v
                , rbiOriginalRhs = rhs
                , rbiLiftedVar = liftedVar
                , rbiLiftedType = liftedTy
                }

    -- Build and emit each lifted function
    for_ bindingInfos $ \info ->
        local (extendEnv' boundVars) (emitLiftedRecBinding captures bindingInfos info)

    -- Rewrite the original site with closure chains
    cont' <- liftClosures' cont
    buildChains captures bindingInfos cont'

{- | Compute captures for a recursive group
Captures = (all free vars) - (bound vars in group) - (globals)
-}
computeGroupCaptures :: Set Core.Var -> Set Core.Var -> [Core.Var]
computeGroupCaptures boundVars allFreeVars =
    Set.toList $
        Set.filter (not . isGlobal) allFreeVars
            `Set.difference` boundVars

-- | Emit a single lifted recursive binding
emitLiftedRecBinding ::
    LiftClosures r =>
    [Core.Var] ->
    [RecBindingInfo] ->
    RecBindingInfo ->
    Eff r ()
emitLiftedRecBinding captures allBindings info = do
    -- Peel all lambda arguments from the RHS
    let (args, coreBody) = peelLam (rbiOriginalRhs info)

    -- Lift the body with all variables in scope
    let innerCtx :: LiftContext -> LiftContext
        innerCtx ctx =
            withParentName (extractVarName (rbiOriginalVar info)) $
                extendEnv' (fromList (captures <> args)) ctx
    body' <- local innerCtx (liftClosures' coreBody)

    -- Insert chains for mutual recursion
    bodyWithFixups <- buildChains captures allBindings body'

    -- Re-wrap: \captures -> \args -> body_with_fixups
    let allArgs = captures <> args
    finalCExpr <- case allArgs of
        [] -> requireCExpr "emitLiftedRecBinding no-arg body" bodyWithFixups
        _ -> do
            let finalBody = mkNestedLam allArgs bodyWithFixups
            requireCExpr "emitLiftedRecBinding final body" finalBody

    tell [(rbiLiftedVar info, finalCExpr)]

-- | Build closure chains for recursive bindings with shared captures
buildChains ::
    LiftClosures r =>
    [Core.Var] ->
    [RecBindingInfo] ->
    Expr Core.Var ->
    Eff r (Expr Core.Var)
buildChains captures bindings cont =
    foldM
        ( \acc info ->
            mkClosureChain
                (rbiOriginalVar info)
                (rbiLiftedVar info)
                captures
                acc
        )
        cont
        bindings
