{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoPatternSynonyms #-}

module Elara.TypeInfer.ConstraintGeneration (
    generateConstraints,
    solveConstraint,
    lookupType,
    -- Exported for testing
    unify,
) where

import Data.Foldable (foldrM)
import Data.Generics.Product (HasType (typed))
import Data.Generics.Wrapped (_Unwrapped)
import Data.Set (member)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Extra (scoped)
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Elara.AST.Generic (Expr (..), Expr' (..))
import Elara.AST.Generic.Common (NoFieldValue (NoFieldValue))
import Elara.AST.Generic.Types (Pattern (..), Pattern' (..), TypedLambdaParam (..))
import Elara.AST.Generic.Types qualified as Syntax
import Elara.AST.Name (Qualified, TypeName, VarName (..))
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Shunted (ShuntedExpr, ShuntedPattern, ShuntedPattern')
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.AST.Typed (TypedExpr, TypedExpr', TypedPattern, TypedPattern')
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Logging (StructuredDebug, debug, debugWith, debugWithResult, logDebug, logDebugWith)
import Elara.Prim (boolName, charName, floatName, intName, mkPrimQual, stringName, unitName)
import Elara.Query qualified
import Elara.Query.Effects (QueryEffects)
import Elara.TypeInfer.Context (ContextStack (..), InferenceContext (..))
import Elara.TypeInfer.Environment (LocalTypeEnvironment, TypeEnvKey (..), TypeEnvironment, addLocalType, lookupLocalVar, lookupTypeMaybe, withLocalType)
import Elara.TypeInfer.Error (UnifyError (..), UnifyErrorKind (..), mkUnifyError, mkUnifyErrorFromConstraint)
import Elara.TypeInfer.Ftv (Ftv (..), Fuv (fuv))
import Elara.TypeInfer.Generalise (generalise)
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type (Constraint (..), Monotype (..), Polytype (..), Substitutable (..), Substitution (..), Type (..), TypeVariable (SkolemVar, UnificationVar), equalityWithContext, functionMonotypeArgs, functionMonotypeResult, monotypeLoc, reduce, simpleEquality, substitution)
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)
import Optics (anyOf)
import Rock qualified

-- | Common effects required for Constraint Generation
type ConstraintGenEffects r loc =
    ( Infer loc r
    , Error (UnifyError loc) :> r
    , loc ~ SourceRegion
    , HasCallStack
    )

-- | Common effects required for Unification
type UnifyEffects r loc =
    ( Error (UnifyError loc) :> r
    , Reader ContextStack :> r
    , StructuredDebug :> r
    , Rock.Rock Elara.Query.Query :> r
    , QueryEffects r
    , loc ~ SourceRegion
    , Pretty loc
    , Eq loc
    , Show loc
    , HasCallStack
    )

{- | Try to extract the function name from an expression for better error messages.
This walks through nested function applications to find the base function name.
-}
extractFunctionName :: ShuntedExpr -> Maybe (Qualified VarName)
extractFunctionName (Expr (Located _ expr', _)) = case expr' of
    Var (Located _ varRef) -> case varRef of
        Global (Located _ qn) -> Just qn
        Local (Located _ ln) -> Nothing -- Local variables don't have qualified names
    FunctionCall fn _ -> extractFunctionName fn -- Recurse through curried applications
    _ -> Nothing

generateConstraints :: ConstraintGenEffects r loc => ShuntedExpr -> Eff r (TypedExpr, Monotype loc)
generateConstraints expr'@(Expr (Located loc _, _)) = do
    (typedExpr', monotype) <- generateConstraints' expr'
    pure (Expr (Located loc typedExpr', monotype), monotype)

{- | Lookup a type in the type environment, querying if necessary.
Looks in the local environment first to avoid unnecessary queries / infinite loops, then queries the global environment if not found locally.
-}
lookupType ::
    ( StructuredDebug :> r
    , QueryEffects r
    , loc ~ SourceRegion
    , Rock.Rock Elara.Query.Query :> r
    , State (TypeEnvironment loc) :> r
    ) =>
    TypeEnvKey loc ->
    Eff r (Type loc)
lookupType key = do
    -- try the local environment first to avoid unnecessary queries / infinite loops
    inEnv <- lookupTypeMaybe key
    case inEnv of
        Just ty -> pure ty
        Nothing -> do
            logDebug ("Type not found in environment, querying: " <> pretty key)
            Rock.fetch (Elara.Query.TypeOf key)

generateConstraints' :: ConstraintGenEffects r loc => ShuntedExpr -> Eff r (TypedExpr', Monotype loc)
generateConstraints' expr' =
    logDebugWith ("generateConstraints: " <> pretty expr') $
        let exprLoc = expr' ^. Syntax.exprLocation
         in case expr' ^. _Unwrapped % _1 % unlocated of
                Int i -> pure (Int i, TypeConstructor exprLoc (mkPrimQual intName) [])
                Float f -> pure (Float f, TypeConstructor exprLoc (mkPrimQual floatName) [])
                String s -> pure (String s, TypeConstructor exprLoc (mkPrimQual stringName) [])
                Char c -> pure (Char c, TypeConstructor exprLoc (mkPrimQual charName) [])
                Unit -> pure (Unit, TypeConstructor exprLoc (mkPrimQual unitName) [])
                Constructor ctorValue@(Located loc name) -> do
                    -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
                    varType <- lookupType (DataConKey $ stripLocation name)

                    (instantiated, typeApps) <- instantiate varType

                    let ctor = (Constructor ctorValue, instantiated)

                    let withApps =
                            foldl'
                                ( \(expr :: TypedExpr', exprType) tv ->
                                    let expr' :: TypedExpr = Expr (Located loc expr, exprType)
                                     in (TypeApplication expr' (TypeVar exprLoc tv), exprType)
                                )
                                ctor
                                typeApps

                    pure withApps

                -- VAR
                Var v'@(Located loc varName) -> do
                    varType <- case varName of
                        Local (Located _ n) -> lookupLocalVar n
                        Global (Located _ n) -> lookupType (TermVarKey n)
                    debug ("Var: " <> pretty varName <> " : " <> pretty varType)
                    -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
                    (instantiated, tyApps) <- instantiate varType

                    let instantiated' = (Var (Located loc (varName, varType)), instantiated)

                    let withApps =
                            foldl'
                                ( \(expr :: TypedExpr', exprType) tv ->
                                    let expr' :: TypedExpr = Expr (Located loc expr, exprType)
                                     in (TypeApplication expr' (TypeVar exprLoc tv), exprType)
                                )
                                instantiated'
                                tyApps

                    pure withApps
                -- ABS
                (Lambda (Located paramLoc (TypedLambdaParam (paramName, _expectedParamType))) body) -> do
                    paramTyVar <- UnificationVar <$> makeUniqueTyVar

                    (typedBody, bodyType) <- withLocalType paramName (Lifted $ TypeVar paramLoc paramTyVar) $ generateConstraints body

                    let functionType = Function exprLoc (TypeVar paramLoc paramTyVar) bodyType

                    pure
                        ( Lambda (Located paramLoc (TypedLambdaParam (paramName, TypeVar paramLoc paramTyVar))) typedBody
                        , functionType
                        )

                -- APP
                FunctionCall e1 e2 -> do
                    (e1', t1) <- generateConstraints e1
                    (e2', t2) <- generateConstraints e2

                    resultTyVar <- UnificationVar <$> makeUniqueTyVar

                    let e1Loc = e1 ^. Syntax.exprLocation
                    let e2Loc = e2 ^. Syntax.exprLocation

                    -- Try to extract the function name for better error messages
                    let fnName = extractFunctionName e1

                    -- Create constraint with context about this being a function application
                    let ctx = Just $ CheckingFunctionArgument 1 fnName exprLoc
                    let equalityConstraint = equalityWithContext exprLoc t1 (Function e1Loc t2 (TypeVar e2Loc resultTyVar)) e1Loc e2Loc ctx
                    debug (pretty equalityConstraint)
                    tell equalityConstraint

                    pure (FunctionCall e1' e2', TypeVar e2Loc resultTyVar)

                -- LET
                {-
                    Q ; Γ ⊢ e1 : τ1 Q ; Γ, (x :τ1) ⊢ e2 : τ2
                    ----------------------------------------------
                            Q ; Γ ⊢ let x = e1 in e2 : τ2

                    (except not quite because lets are recursive)
                    -}
                LetIn (Located loc varName) NoFieldValue varExpr body -> do
                    recursiveVar <- UnificationVar <$> makeUniqueTyVar

                    -- Q ; Γ ⊢ e1 : τ1
                    (typedVarExpr, varType) <-
                        withLocalType varName (Lifted $ TypeVar loc recursiveVar) $
                            generateConstraints varExpr

                    let recursiveConstraint = simpleEquality exprLoc (TypeVar loc recursiveVar) varType
                    tell recursiveConstraint

                    -- TODO: we need to check if e1 is closed here before generalising _everything_

                    let isRecursive =
                            anyOf
                                (cosmosOf gplate % _Unwrapped % _1 % unlocated % _Ctor' @"Var" % unlocated % _Ctor' @"Local" % unlocated)
                                (== varName)
                                varExpr

                    debug ("isRecursive?: " <> pretty isRecursive)
                    maybeGeneralised <-
                        if not isRecursive
                            then do
                                generalised <- generalise varType
                                debug (pretty varType <> " -> generalised: " <> pretty generalised)
                                pure (Polytype generalised)
                            else pure (Lifted varType)

                    (typedBody, bodyType) <-
                        withLocalType varName maybeGeneralised $
                            generateConstraints body

                    pure (LetIn (Located loc varName) NoFieldValue typedVarExpr typedBody, bodyType)

                -- IF
                If cond then' else' -> do
                    (typedCond, condType) <- generateConstraints cond
                    (typedThen, thenType) <- generateConstraints then'
                    (typedElse, elseType) <- generateConstraints else'

                    let condLoc = cond ^. Syntax.exprLocation
                    let thenLoc = then' ^. Syntax.exprLocation
                    let elseLoc = else' ^. Syntax.exprLocation

                    -- Condition must be Bool
                    let condCtx = Just $ CheckingIfCondition exprLoc
                    let condConstraint = equalityWithContext condLoc condType (TypeConstructor condLoc (mkPrimQual boolName) []) condLoc condLoc condCtx
                    tell condConstraint

                    -- Both branches must have the same type
                    let branchCtx = Just $ CheckingIfBranches thenLoc elseLoc
                    let branchConstraint = equalityWithContext thenLoc thenType elseType thenLoc elseLoc branchCtx
                    tell branchConstraint

                    pure (If typedCond typedThen typedElse, thenType)
                TypeApplication e ty -> error "i dont know what to do with type applications yet sorry"
                -- MATCH
                -- (match (e: τ) with { p1 -> e1; ...; pn -> en }) : τr
                Match e cases -> do
                    -- Q ; Γ ⊢ e : τ
                    (typedE, eType) <- generateConstraints e

                    -- τr
                    resultTyVar <- UnificationVar <$> makeUniqueTyVar

                    let eLoc = e ^. Syntax.exprLocation

                    cases' <- for (zip [1 ..] cases) $ \(branchIdx, (pattern, body)) -> scoped @(LocalTypeEnvironment _) $ do
                        -- Q ; Γ ⊢ p1 : τ1
                        (typedPattern, patternType) <- generatePatternConstraints pattern eType

                        -- Q ; Γ ⊢ e1 : τ2
                        (typedBody, bodyType) <- generateConstraints body

                        -- τ2 ~ τr (all branches must have same type)
                        let bodyLoc = body ^. Syntax.exprLocation
                        let branchCtx = Just $ CheckingMatchBranch branchIdx bodyLoc
                        tell (equalityWithContext bodyLoc bodyType (TypeVar eLoc resultTyVar) bodyLoc eLoc branchCtx)

                        pure (typedPattern, typedBody)

                    pure (Match typedE cases', TypeVar eLoc resultTyVar)
                Block exprs -> do
                    vals <- for exprs $ \expr -> generateConstraints expr

                    let exprs = fmap fst vals

                    let exprTypes = fmap snd vals

                    pure (Syntax.Block exprs, last exprTypes)
                Let (Located loc varName) NoFieldValue varExpr -> do
                    recursiveVar <- UnificationVar <$> makeUniqueTyVar
                    (typedVarExpr, varType) <- withLocalType varName (Lifted $ TypeVar loc recursiveVar) $ generateConstraints varExpr

                    let recursiveConstraint = simpleEquality exprLoc (TypeVar loc recursiveVar) varType
                    tell recursiveConstraint

                    modify (addLocalType varName (Lifted varType))

                    pure (Let (Located loc varName) NoFieldValue typedVarExpr, varType)

{- | Generate constraints for a pattern, returning the typed pattern and the type of the pattern, and generating constraints for the pattern
We define the type of a pattern as the type of values it can match against, rather than its "signature"

For example, in the case of a simple option type @type Option a = Some a | None@,
we say that the pattern `Some x` has type `Option a` rather than @a -> Option a@
-}
generatePatternConstraints :: ConstraintGenEffects r loc => ShuntedPattern -> Monotype loc -> Eff r (TypedPattern, Monotype loc)
generatePatternConstraints (Pattern (Located loc pattern', expectedType)) over = debugWithResult ("generatePatternConstraints: " <> pretty pattern') $ do
    (typedPattern', monotype) <- generatePatternConstraints' pattern' over
    let patternCtx = Just $ CheckingPattern loc
    tell (equalityWithContext loc monotype over loc (monotypeLoc over) patternCtx)
    pure (Pattern (Located loc typedPattern', monotype), monotype)

generatePatternConstraints' ::
    ConstraintGenEffects r loc =>
    ShuntedPattern' ->
    {- | the type of the pattern we are matching against
    For example if we have `match (x : Option Int) with { Some y -> y }` then `over` would be `Option Int`
    This is necessary for `WildcardPattern` and `VarPattern` to know what type they should be
    -}
    Monotype loc ->
    Eff r (TypedPattern', Monotype loc)
generatePatternConstraints' pattern' over =
    debugWithResult ("generatePatternConstraints: " <> pretty pattern') $
        let patternLoc = monotypeLoc over
         in case pattern' of
                WildcardPattern -> pure (WildcardPattern, over)
                UnitPattern -> pure (UnitPattern, TypeConstructor patternLoc (mkPrimQual "Unit") [])
                IntegerPattern i -> pure (IntegerPattern i, TypeConstructor patternLoc (mkPrimQual intName) [])
                FloatPattern f -> pure (FloatPattern f, TypeConstructor patternLoc (mkPrimQual "Float") [])
                StringPattern s -> pure (StringPattern s, TypeConstructor patternLoc (mkPrimQual stringName) [])
                CharPattern c -> pure (CharPattern c, TypeConstructor patternLoc (mkPrimQual charName) [])
                VarPattern (Located loc varName) -> do
                    varType <- UnificationVar <$> makeUniqueTyVar
                    modify (addLocalType (NormalVarName <$> varName) (Lifted $ TypeVar loc varType))
                    -- tell (Equality loc over (TypeVar loc varType))

                    pure (VarPattern (Located loc (NormalVarName <$> varName)), TypeVar loc varType)
                ConstructorPattern ctor'@(Located loc ctor) args -> do
                    -- lookup the signature of the constructor
                    t <- lookupType (DataConKey ctor)
                    debug ("generatePatternConstraints (ConstructorPattern): " <> pretty ctor <> " :: " <> pretty t)

                    -- if we have a constructor @Ctor : x -> y -> Z@
                    -- and pattern @Ctor a b@
                    -- we need to generate @a : x@ and @b : y@
                    -- we do this by making one big constraint out of fresh type variables @a_1 -> @b_1 -> ... -> Z@
                    -- and emitting a single equality constraint @x -> y -> Z = a_1 -> b_1 -> ... -> Z@

                    (instantiatedT, typeApps) <- instantiate t
                    debug $ "instantiatedT: " <> pretty instantiatedT
                    debug $ "tyApps: " <> pretty typeApps
                    let argTys = functionMonotypeArgs instantiatedT
                    let res = functionMonotypeResult instantiatedT
                    debug $ "argTys: " <> pretty argTys
                    debug $ "res: " <> pretty res

                    -- tell (Equality patternLoc over res)
                    when (length argTys /= length args) $ do
                        ctx <- ask @ContextStack
                        -- Create an error with the pattern arity mismatch
                        throwError $
                            mkUnifyError
                                (PatternArityMismatch ctor (length argTys) (length args))
                                instantiatedT -- expected type (what ctor produces)
                                over -- actual type (what we're matching against)
                                patternLoc
                                ctx
                    argPats <- for (zip args argTys) $ \(arg, argTy) -> do
                        generatePatternConstraints arg argTy

                    pure (ConstructorPattern ctor' (fmap fst argPats), res)

instantiate :: forall r loc. ConstraintGenEffects r loc => Type loc -> Eff r (Monotype loc, [TypeVariable])
instantiate (Lifted t) = pure (t, [])
instantiate pt@(Polytype (Forall loc tyVars constraint t)) = debugWith ("instantiate: " <> pretty pt) $ do
    fresh <- mapM (const (UnificationVar <$> makeUniqueTyVar)) tyVars
    let substitution = Substitution $ fromList $ zip (fmap (view typed) tyVars) (fmap (TypeVar loc) fresh)
    let instantiatedConstraint =
            substituteAll substitution constraint
        instantiatedMonotype =
            substituteAll substitution t

    tell instantiatedConstraint
    pure (instantiatedMonotype, fresh)

solveConstraint ::
    (UnifyEffects r a, Monoid (Constraint a)) =>
    Constraint a -> Set UniqueTyVar -> Constraint a -> Eff r (Constraint a, Substitution a)
solveConstraint given tch wanted = do
    (residual, unifier) <- simplifyConstraint given tch wanted

    let residual' = reduce residual

    pure (residual', unifier)

simplifyConstraint ::
    (UnifyEffects r loc, Monoid (Constraint loc)) =>
    Constraint loc -> Set UniqueTyVar -> Constraint loc -> Eff r (Constraint loc, Substitution loc)
simplifyConstraint given tch wanted = debugWithResult ("simplifyConstraint: " <> pretty (given, wanted)) $ do
    givenSubst <- reduceGiven given
    debug ("simplifyConstraint: givenSubst: " <> pretty givenSubst)
    runReader tch (solve (substituteAll givenSubst wanted))

reduceGiven ::
    (UnifyEffects r loc, Monoid (Constraint loc)) =>
    Constraint loc -> Eff r (Substitution loc)
reduceGiven eq@Equality{eqLeft = a, eqRight = b} = debugWithResult ("reduceGiven: " <> pretty eq) $ unifyGiven (Just eq) a b
reduceGiven conj@(Conjunction _ a b) = debugWithResult ("reduceGiven: " <> pretty conj) $ do
    s1 <- reduceGiven a
    s2 <- reduceGiven (substituteAll s1 b)
    pure (s1 <> s2)
reduceGiven EmptyConstraint{} = pure mempty

solve ::
    (UnifyEffects r loc, Monoid (Constraint loc), Reader (Set UniqueTyVar) :> r) =>
    Pretty loc =>
    Constraint loc ->
    Eff r (Constraint loc, Substitution loc)
solve constraint@Equality{eqLeft = a, eqRight = b} = do
    let ?constraint = Just constraint in unify a b
solve (Conjunction _ a b) = do
    -- a lot of this is duplicated from unifyMany which is a bit annoying
    (c1, s1) <- solve a
    -- apply s1 to b before unifying
    (c2, s2) <- solve (substituteAll s1 b)
    pure (c1 <> c2, s1 <> s2)
solve EmptyConstraint{} = pure mempty

unifyGiven ::
    (UnifyEffects r loc, Monoid (Constraint loc)) =>
    Maybe (Constraint loc) -> Monotype loc -> Monotype loc -> Eff r (Substitution loc)
unifyGiven _ (TypeVar _ a) b = bindGiven a b
unifyGiven _ a (TypeVar _ b) = bindGiven b a
unifyGiven constraint (Function _ a b) (Function _ c d) = unifyGivenMany constraint [a, b] [c, d]
unifyGiven constraint t1@(TypeConstructor l1 a as) t2@(TypeConstructor l2 b bs)
    | a == b =
        if length as /= length bs
            then do
                ctx <- ask @ContextStack
                throwError $ mkUnifyError (ArityMismatch (length as) (length bs)) t1 t2 l1 ctx
            else unifyGivenMany constraint as bs
    | otherwise = do
        expandedA <- expandAlias a as
        expandedB <- expandAlias b bs
        case (expandedA, expandedB) of
            (Just a', _) -> unifyGiven constraint a' t2
            (_, Just b') -> unifyGiven constraint t1 b'
            (Nothing, Nothing) -> do
                ctx <- ask @ContextStack
                case constraint of
                    Just c -> throwError $ mkUnifyErrorFromConstraint (TypeConstructorMismatch a b) t1 t2 c ctx
                    Nothing -> throwError $ mkUnifyError (TypeConstructorMismatch a b) t1 t2 l1 ctx
unifyGiven constraint a b = do
    ctx <- ask @ContextStack
    case constraint of
        Just c -> throwError $ mkUnifyErrorFromConstraint TypeMismatch a b c ctx
        Nothing -> throwError $ mkUnifyError TypeMismatch a b (monotypeLoc a) ctx

unifyGivenMany constraint xs ys = foldrM go mempty (zip xs ys)
  where
    go (a, b) s = do
        s' <- unifyGiven constraint (substituteAll s a) (substituteAll s b)
        pure (s <> s')

unify ::
    forall loc r.
    ( UnifyEffects r loc
    , Monoid (Constraint loc)
    , ?constraint :: Maybe (Constraint loc)
    , Reader (Set UniqueTyVar) :> r
    ) =>
    Monotype loc ->
    Monotype loc ->
    Eff r (Constraint loc, Substitution loc)
unify a b = do
    logDebug ("unify: " <> pretty (a, b))
    unify' a b
  where
    unify' ::
        Monotype loc -> Monotype loc -> Eff r (Constraint loc, Substitution loc)

    unify' (TypeVar _ a) (TypeVar _ b) | a == b = pure (mempty, mempty)
    unify' (TypeVar _ (UnificationVar a)) b = unifyVar a b
    unify' a (TypeVar _ (UnificationVar b)) = unifyVar b a
    unify' t1@(TypeConstructor l1 a as) t2@(TypeConstructor l2 b bs)
        | a == b =
            if length as /= length bs
                then do
                    ctx <- ask @ContextStack
                    throwError $ mkUnifyError (ArityMismatch (length as) (length bs)) t1 t2 l1 ctx
                else unifyMany as bs
        | otherwise = do
            expandedA <- expandAlias a as
            expandedB <- expandAlias b bs
            logDebug $ "unify: trying to expand aliases: " <> pretty (expandedA, expandedB)
            case (expandedA, expandedB) of
                (Just a', _) -> do
                    logDebug $ "unify: expanded alias for " <> pretty a <> ": " <> pretty a'
                    unify a' t2
                (_, Just b') -> do
                    logDebug $ "unify: expanded alias for " <> pretty b <> ": " <> pretty b'
                    unify t1 b'
                (Nothing, Nothing) -> do
                    ctx <- ask @ContextStack
                    case ?constraint of
                        Just c -> throwError $ mkUnifyErrorFromConstraint (TypeConstructorMismatch a b) t1 t2 c ctx
                        Nothing -> throwError $ mkUnifyError (TypeConstructorMismatch a b) t1 t2 l1 ctx
    unify' t1@(TypeConstructor _ a as) t2 = do
        expanded <- expandAlias a as
        case expanded of
            Just t1' -> unify' t1' t2
            Nothing -> do
                ctx <- ask @ContextStack
                case ?constraint of
                    Just c -> throwError $ mkUnifyErrorFromConstraint TypeMismatch t1 t2 c ctx
                    Nothing -> throwError $ mkUnifyError TypeMismatch t1 t2 (monotypeLoc t1) ctx
    unify' t1 t2@(TypeConstructor _ b bs) = do
        expanded <- expandAlias b bs
        case expanded of
            Just t2' -> unify' t1 t2'
            Nothing -> do
                ctx <- ask @ContextStack
                case ?constraint of
                    Just c -> throwError $ mkUnifyErrorFromConstraint TypeMismatch t1 t2 c ctx
                    Nothing -> throwError $ mkUnifyError TypeMismatch t1 t2 (monotypeLoc t1) ctx
    unify' (Function _ a b) (Function _ c d) = unifyMany [a, b] [c, d]
    unify' a b = do
        ctx <- ask @ContextStack
        case ?constraint of
            Just c -> throwError $ mkUnifyErrorFromConstraint TypeMismatch a b c ctx
            Nothing -> throwError $ mkUnifyError TypeMismatch a b (monotypeLoc a) ctx

expandAlias ::
    ( UnifyEffects r loc
    , loc ~ SourceRegion
    ) =>
    Qualified TypeName ->
    [Monotype loc] ->
    Eff r (Maybe (Monotype loc))
expandAlias name args = do
    aliasDef <- Rock.fetch (Elara.Query.GetTypeAlias name)

    case aliasDef of
        Just (params, body) -> do
            if length params /= length args
                then do
                    error "TODO: throw an arity mismatch error here"
                else do
                    let s = Substitution $ fromList (zip (view typed <$> params) args)
                    case body of
                        Lifted m -> pure (Just $ substituteAll s m)
                        Polytype{} -> throwError $ PolytypeAlias (params, body)
        Nothing -> pure Nothing

bindGiven ::
    (StructuredDebug :> r, Reader ContextStack :> r, Error (UnifyError loc) :> r, Show loc) =>
    TypeVariable -> Monotype loc -> Eff r (Substitution loc)
bindGiven a t =
    if member a (ftv t)
        then do
            ctx <- ask @ContextStack
            let tvType = TypeVar (monotypeLoc t) a
            throwError $ mkUnifyError (OccursCheck a) tvType t (monotypeLoc t) ctx
        else pure (substitution (tvValue a, t))
  where
    tvValue (UnificationVar a) = a
    tvValue (SkolemVar a) = a

unifyVar ::
    forall loc r.
    ( StructuredDebug :> r
    , Reader ContextStack :> r
    , Error (UnifyError loc) :> r
    , Reader (Set UniqueTyVar) :> r
    , Show loc
    , Eq loc
    ) =>
    UniqueTyVar ->
    Monotype loc ->
    Eff r (Constraint loc, Substitution loc)
unifyVar a t = do
    debug $ "bind " <> pretty a <> " to " <> pretty t
    bindVar a t
  where
    bindVar :: UniqueTyVar -> Monotype loc -> Eff r (Constraint loc, Substitution loc)
    bindVar tv t | member tv (fuv t) = do
        ctx <- ask @ContextStack
        let tvType = TypeVar (monotypeLoc t) (UnificationVar tv)
        throwError $ mkUnifyError (OccursCheck (UnificationVar tv)) tvType t (monotypeLoc t) ctx
    bindVar tv t = do
        tch <- ask @(Set UniqueTyVar)
        debug ("bindVar " <> pretty tv <> " to " <> pretty t)
        if member tv tch
            then pure (EmptyConstraint (monotypeLoc t), substitution (tv, t))
            else pure (simpleEquality (monotypeLoc t) (TypeVar (monotypeLoc t) $ UnificationVar tv) t, mempty)

unifyMany ::
    ( UnifyEffects r loc
    , Monoid (Constraint loc)
    , ?constraint :: Maybe (Constraint loc)
    , Reader (Set UniqueTyVar) :> r
    ) =>
    [Monotype loc] ->
    [Monotype loc] ->
    Eff r (Constraint loc, Substitution loc)
unifyMany [] [] = pure (mempty, mempty)
unifyMany [] (b : _) = do
    ctx <- ask @ContextStack
    throwError $ mkUnifyError UnifyMismatch b b (monotypeLoc b) ctx
unifyMany (a : _) [] = do
    ctx <- ask @ContextStack
    throwError $ mkUnifyError UnifyMismatch a a (monotypeLoc a) ctx
unifyMany (a : as) (b : bs) = do
    (c1, s1) <- unify a b
    (c2, s2) <- unifyMany (fmap (substituteAll s1) as) (fmap (substituteAll s1) bs)
    pure (c1 <> c2, s1 <> s2)
