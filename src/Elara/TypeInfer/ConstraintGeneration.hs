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
import Data.Set (member)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.State.Extra (scoped)
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Elara.AST.Name (Qualified, TypeName, VarName (..))
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Renamed (TypedLambdaParam (..))
import Elara.AST.Phases.Shunted (ShuntedExpr, ShuntedPattern, ShuntedPattern')
import Elara.AST.Phases.Typed (Typed, TypedExpr, TypedExpr', TypedPattern, TypedPattern')
import Elara.AST.Region (Located (Located), SourceRegion)
import Elara.AST.Types qualified as New
import Elara.AST.VarRef
import Elara.Data.Kind (ElaraKind (..))
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import Elara.Logging (StructuredDebug, debugWithResult, logDebug, logDebugWith)
import Elara.Prim (KnownType (..), KnownTypeInfo (..), OpaquePrim (..), WiredInPrim (..), knownTypeInfo)
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
extractFunctionName (New.Expr _ _ expr') = case expr' of
    New.EVar NoExtension (Located _ varRef) -> case varRef of
        Global (Located _ qn) -> Just qn
        Local _ -> Nothing
    New.EApp NoExtension fn _ -> extractFunctionName fn
    _ -> Nothing

generateConstraints :: ConstraintGenEffects r loc => ShuntedExpr -> Eff r (TypedExpr, Monotype loc)
generateConstraints expr'@(New.Expr loc _ _) = do
    (typedExpr', monotype) <- generateConstraints' expr'
    pure (New.Expr loc monotype typedExpr', monotype)

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

-- | Helper to create an AST type from a TypeVariable for type application in the Typed phase
mkTyAppArg :: SourceRegion -> TypeVariable -> New.Type SourceRegion Typed
mkTyAppArg loc (UnificationVar tv) = New.Type loc TypeKind (New.TVar (Located loc tv))
mkTyAppArg loc (SkolemVar tv) = New.Type loc TypeKind (New.TVar (Located loc tv))

-- | Check if an expression references a given variable name (for recursion detection)
isRecursiveIn :: Unique VarName -> ShuntedExpr -> Bool
isRecursiveIn vn = go
  where
    go (New.Expr _ _ e') = case e' of
        New.EVar _ (Located _ (Local (Located _ n))) -> n == vn
        New.EVar _ _ -> False
        New.ELam _ _ body -> go body
        New.EApp _ e1 e2 -> go e1 || go e2
        New.ELetIn _ _ e1 e2 -> go e1 || go e2
        New.ELet _ _ e1 -> go e1
        New.EIf c t f -> go c || go t || go f
        New.EMatch e cases -> go e || any (\(_, rhs) -> go rhs) cases
        New.EBlock exprs -> any go exprs
        New.ETyApp e _ -> go e
        New.EAnn e _ -> go e
        _ -> False

-- | Get the location from a Shunted expression
exprLocation :: ShuntedExpr -> SourceRegion
exprLocation (New.Expr loc _ _) = loc

generateConstraints' :: ConstraintGenEffects r loc => ShuntedExpr -> Eff r (TypedExpr', Monotype loc)
generateConstraints' expr' =
    logDebugWith ("generateConstraints: <expr at " <> pretty (exprLocation expr') <> ">") $
        let New.Expr exprLoc _ exprBody = expr'
         in case exprBody of
                New.EInt i -> pure (New.EInt i, TypeConstructor exprLoc (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) [])
                New.EFloat f -> pure (New.EFloat f, TypeConstructor exprLoc (knownQualified (knownTypeInfo (KnownOpaque PrimFloat))) [])
                New.EString s -> pure (New.EString s, TypeConstructor exprLoc (knownQualified (knownTypeInfo (KnownOpaque PrimString))) [])
                New.EChar c -> pure (New.EChar c, TypeConstructor exprLoc (knownQualified (knownTypeInfo (KnownOpaque PrimChar))) [])
                New.EUnit -> pure (New.EUnit, TypeConstructor exprLoc (knownQualified (knownTypeInfo (KnownWiredIn WiredInUnit))) [])
                New.ECon NoExtension ctorValue@(Located loc name) -> do
                    -- (v:forall a.Q1 => t1) in G
                    varType <- lookupType (DataConKey name)

                    (instantiated, typeApps) <- instantiate varType

                    let ctor = (New.ECon NoExtension ctorValue, instantiated)

                    let withApps =
                            foldl'
                                ( \(expr :: TypedExpr', exprType) tv ->
                                    let expr'' :: TypedExpr = New.Expr loc exprType expr
                                     in (New.ETyApp expr'' (mkTyAppArg exprLoc tv), exprType)
                                )
                                ctor
                                typeApps

                    pure withApps

                -- VAR
                New.EVar NoExtension v'@(Located loc varName) -> do
                    varType <- case varName of
                        Local (Located _ n) -> lookupLocalVar n
                        Global (Located _ n) -> lookupType (TermVarKey n)
                    logDebug ("Var: " <> pretty varName <> " : " <> pretty varType)
                    -- (v:forall a.Q1 => t1) in G
                    (instantiated, tyApps) <- instantiate varType

                    let instantiated' = (New.EVar varType v', instantiated)

                    let withApps =
                            foldl'
                                ( \(expr :: TypedExpr', exprType) tv ->
                                    let expr'' :: TypedExpr = New.Expr loc exprType expr
                                     in (New.ETyApp expr'' (mkTyAppArg exprLoc tv), exprType)
                                )
                                instantiated'
                                tyApps

                    pure withApps
                -- ABS
                New.ELam NoExtension (TypedLambdaParam paramName _expectedParamType) body -> do
                    paramTyVar <- UnificationVar <$> makeUniqueTyVar

                    let paramLoc = exprLoc -- use expression location for param
                    (typedBody, bodyType) <- withLocalType paramName (Lifted $ TypeVar paramLoc paramTyVar) $ generateConstraints body

                    let functionType = Function exprLoc (TypeVar paramLoc paramTyVar) bodyType

                    pure
                        ( New.ELam NoExtension (TypedLambdaParam paramName (TypeVar paramLoc paramTyVar)) typedBody
                        , functionType
                        )

                -- APP
                New.EApp NoExtension e1 e2 -> do
                    (e1', t1) <- generateConstraints e1
                    (e2', t2) <- generateConstraints e2

                    resultTyVar <- UnificationVar <$> makeUniqueTyVar

                    let e1Loc = exprLocation e1
                    let e2Loc = exprLocation e2

                    -- Try to extract the function name for better error messages
                    let fnName = extractFunctionName e1

                    -- Create constraint with context about this being a function application
                    let ctx = Just $ CheckingFunctionArgument 1 fnName exprLoc
                    let equalityConstraint = equalityWithContext exprLoc t1 (Function e1Loc t2 (TypeVar e2Loc resultTyVar)) e1Loc e2Loc ctx
                    logDebug (pretty equalityConstraint)
                    tell equalityConstraint

                    pure (New.EApp NoExtension e1' e2', TypeVar e2Loc resultTyVar)

                -- LET
                {-
                    Q ; G |- e1 : t1 Q ; G, (x :t1) |- e2 : t2
                    ----------------------------------------------
                            Q ; G |- let x = e1 in e2 : t2

                    (except not quite because lets are recursive)
                    -}
                New.ELetIn NoExtension (Located loc varName) varExpr body -> do
                    recursiveVar <- UnificationVar <$> makeUniqueTyVar

                    -- Q ; G |- e1 : t1
                    (typedVarExpr, varType) <-
                        withLocalType varName (Lifted $ TypeVar loc recursiveVar) $
                            generateConstraints varExpr

                    let recursiveConstraint = simpleEquality exprLoc (TypeVar loc recursiveVar) varType
                    tell recursiveConstraint

                    -- TODO: we need to check if e1 is closed here before generalising _everything_

                    let isRecursive = isRecursiveIn varName varExpr

                    logDebug ("isRecursive?: " <> pretty isRecursive)
                    maybeGeneralised <-
                        if not isRecursive
                            then do
                                generalised <- generalise varType
                                logDebug (pretty varType <> " -> generalised: " <> pretty generalised)
                                pure (Polytype generalised)
                            else pure (Lifted varType)

                    (typedBody, bodyType) <-
                        withLocalType varName maybeGeneralised $
                            generateConstraints body

                    pure (New.ELetIn NoExtension (Located loc varName) typedVarExpr typedBody, bodyType)

                -- IF
                New.EIf cond then' else' -> do
                    (typedCond, condType) <- generateConstraints cond
                    (typedThen, thenType) <- generateConstraints then'
                    (typedElse, elseType) <- generateConstraints else'

                    let condLoc = exprLocation cond
                    let thenLoc = exprLocation then'
                    let elseLoc = exprLocation else'

                    -- Condition must be Bool
                    let condCtx = Just $ CheckingIfCondition exprLoc
                    let condConstraint = equalityWithContext condLoc condType (TypeConstructor condLoc (knownQualified (knownTypeInfo (KnownWiredIn WiredInBool))) []) condLoc condLoc condCtx
                    tell condConstraint

                    -- Both branches must have the same type
                    let branchCtx = Just $ CheckingIfBranches thenLoc elseLoc
                    let branchConstraint = equalityWithContext thenLoc thenType elseType thenLoc elseLoc branchCtx
                    tell branchConstraint

                    pure (New.EIf typedCond typedThen typedElse, thenType)
                New.ETyApp _ _ -> error "i dont know what to do with type applications yet sorry"
                -- MATCH
                -- (match (e: t) with { p1 -> e1; ...; pn -> en }) : tr
                New.EMatch e cases -> do
                    -- Q ; G |- e : t
                    (typedE, eType) <- generateConstraints e

                    -- tr
                    resultTyVar <- UnificationVar <$> makeUniqueTyVar

                    let eLoc = exprLocation e

                    cases' <- for (zip [1 ..] cases) $ \(branchIdx, (pattern, body)) -> scoped @(LocalTypeEnvironment _) $ do
                        -- Q ; G |- p1 : t1
                        (typedPattern, _patternType) <- generatePatternConstraints pattern eType

                        -- Q ; G |- e1 : t2
                        (typedBody, bodyType) <- generateConstraints body

                        -- t2 ~ tr (all branches must have same type)
                        let bodyLoc = exprLocation body
                        let branchCtx = Just $ CheckingMatchBranch branchIdx bodyLoc
                        tell (equalityWithContext bodyLoc bodyType (TypeVar eLoc resultTyVar) bodyLoc eLoc branchCtx)

                        pure (typedPattern, typedBody)

                    pure (New.EMatch typedE cases', TypeVar eLoc resultTyVar)
                New.EBlock exprs -> do
                    vals <- for exprs $ \expr -> generateConstraints expr

                    let exprs' = fmap fst vals

                    let exprTypes = fmap snd vals

                    pure (New.EBlock exprs', last exprTypes)
                New.ELet NoExtension (Located loc varName) varExpr -> do
                    recursiveVar <- UnificationVar <$> makeUniqueTyVar
                    (typedVarExpr, varType) <- withLocalType varName (Lifted $ TypeVar loc recursiveVar) $ generateConstraints varExpr

                    let recursiveConstraint = simpleEquality exprLoc (TypeVar loc recursiveVar) varType
                    tell recursiveConstraint

                    modify (addLocalType varName (Lifted varType))

                    pure (New.ELet NoExtension (Located loc varName) typedVarExpr, varType)
                New.EAnn _ _ -> error "TODO: EAnn constraint generation"
                New.EExtension v -> absurd v

{- | Generate constraints for a pattern, returning the typed pattern and the type of the pattern, and generating constraints for the pattern
We define the type of a pattern as the type of values it can match against, rather than its "signature"

For example, in the case of a simple option type @type Option a = Some a | None@,
we say that the pattern `Some x` has type `Option a` rather than @a -> Option a@
-}
generatePatternConstraints :: ConstraintGenEffects r loc => ShuntedPattern -> Monotype loc -> Eff r (TypedPattern, Monotype loc)
generatePatternConstraints (New.Pattern loc _expectedType pattern') over = logDebugWith ("generatePatternConstraints: <pattern at " <> pretty loc <> ">") $ do
    (typedPattern', monotype) <- generatePatternConstraints' pattern' over
    let patternCtx = Just $ CheckingPattern loc
    tell (equalityWithContext loc monotype over loc (monotypeLoc over) patternCtx)
    pure (New.Pattern loc monotype typedPattern', monotype)

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
    logDebugWith "generatePatternConstraints': <pattern>" $
        let patternLoc = monotypeLoc over
         in case pattern' of
                New.PWildcard -> pure (New.PWildcard, over)
                New.PUnit -> pure (New.PUnit, TypeConstructor patternLoc (knownQualified (knownTypeInfo (KnownWiredIn WiredInUnit))) [])
                New.PInt i -> pure (New.PInt i, TypeConstructor patternLoc (knownQualified (knownTypeInfo (KnownOpaque PrimInt))) [])
                New.PFloat f -> pure (New.PFloat f, TypeConstructor patternLoc (knownQualified (knownTypeInfo (KnownOpaque PrimFloat))) [])
                New.PString s -> pure (New.PString s, TypeConstructor patternLoc (knownQualified (knownTypeInfo (KnownOpaque PrimString))) [])
                New.PChar c -> pure (New.PChar c, TypeConstructor patternLoc (knownQualified (knownTypeInfo (KnownOpaque PrimChar))) [])
                New.PVar (Located loc varName) -> do
                    varType <- UnificationVar <$> makeUniqueTyVar
                    modify (addLocalType varName (Lifted $ TypeVar loc varType))

                    pure (New.PVar (Located loc varName), TypeVar loc varType)
                New.PCon ctor'@(Located _loc ctor) args -> do
                    -- lookup the signature of the constructor
                    t <- lookupType (DataConKey ctor)
                    logDebug ("generatePatternConstraints (ConstructorPattern): " <> pretty ctor <> " :: " <> pretty t)

                    -- if we have a constructor @Ctor : x -> y -> Z@
                    -- and pattern @Ctor a b@
                    -- we need to generate @a : x@ and @b : y@
                    -- we do this by making one big constraint out of fresh type variables @a_1 -> @b_1 -> ... -> Z@
                    -- and emitting a single equality constraint @x -> y -> Z = a_1 -> b_1 -> ... -> Z@

                    (instantiatedT, typeApps) <- instantiate t
                    logDebug $ "instantiatedT: " <> pretty instantiatedT
                    logDebug $ "tyApps: " <> pretty typeApps
                    let argTys = functionMonotypeArgs instantiatedT
                    let res = functionMonotypeResult instantiatedT
                    logDebug $ "argTys: " <> pretty argTys
                    logDebug $ "res: " <> pretty res

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

                    pure (New.PCon ctor' (fmap fst argPats), res)
                New.PExtension v -> absurd v

instantiate :: forall r loc. ConstraintGenEffects r loc => Type loc -> Eff r (Monotype loc, [TypeVariable])
instantiate (Lifted t) = pure (t, [])
instantiate pt@(Polytype (Forall loc tyVars constraint t)) = logDebugWith ("instantiate: " <> pretty pt) $ do
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
    logDebug ("simplifyConstraint: givenSubst: " <> pretty givenSubst)
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
unifyGiven constraint t1@(TypeConstructor l1 a as) t2@(TypeConstructor _ b bs)
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
    unify' t1@(TypeConstructor l1 a as) t2@(TypeConstructor _l2 b bs)
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
    logDebug $ "bind " <> pretty a <> " to " <> pretty t
    bindVar a t
  where
    bindVar :: UniqueTyVar -> Monotype loc -> Eff r (Constraint loc, Substitution loc)
    bindVar tv t | member tv (fuv t) = do
        ctx <- ask @ContextStack
        let tvType = TypeVar (monotypeLoc t) (UnificationVar tv)
        throwError $ mkUnifyError (OccursCheck (UnificationVar tv)) tvType t (monotypeLoc t) ctx
    bindVar tv t = do
        tch <- ask @(Set UniqueTyVar)
        logDebug ("bindVar " <> pretty tv <> " to " <> pretty t)
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
