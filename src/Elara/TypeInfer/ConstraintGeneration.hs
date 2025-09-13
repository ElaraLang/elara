{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoPatternSynonyms #-}

module Elara.TypeInfer.ConstraintGeneration where

import Data.Foldable (foldrM)
import Data.Generics.Product (HasType (typed))
import Data.Generics.Sum (AsAny (_As))
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
import Elara.AST.Name (Qualified, VarName (..))
import Elara.AST.Region (Located (Located), SourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.AST.Shunted (ShuntedExpr, ShuntedExpr', ShuntedPattern, ShuntedPattern')
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.AST.Typed (TypedExpr, TypedExpr', TypedPattern, TypedPattern')
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Error (ReportableError (..), defaultReport, writeReport)
import Elara.Logging (StructuredDebug, debug, debugWith, debugWithResult)
import Elara.Prim (boolName, mkPrimQual)
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects, QueryEffects)
import Elara.TypeInfer.Environment (LocalTypeEnvironment, TypeEnvKey (..), TypeEnvironment, addLocalType, lookupLocalVar, lookupTypeMaybe, withLocalType)
import Elara.TypeInfer.Ftv (Ftv (..), Fuv (fuv))
import Elara.TypeInfer.Generalise (generalise)
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type (Constraint (..), DataCon, Monotype (..), Polytype (..), Scalar (..), Substitutable (..), Substitution (..), Type (..), TypeVariable (SkolemVar, UnificationVar), constraintLoc, functionMonotypeArgs, functionMonotypeResult, monotypeLoc, reduce, substitution)
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)
import Error.Diagnose
import Optics (anyOf)
import Optics.Fold (universeOf)
import Rock qualified

generateConstraints :: (Infer SourceRegion r, Error (UnifyError SourceRegion) :> r) => ShuntedExpr -> Eff r (TypedExpr, Monotype SourceRegion)
generateConstraints expr'@(Expr (Located loc _, _)) = do
    (typedExpr', monotype) <- generateConstraints' expr'
    pure (Expr (Located loc typedExpr', monotype), monotype)

lookupType ::
    QueryEffects r =>
    Rock.Rock Elara.Query.Query :> r =>
    State (TypeEnvironment SourceRegion) :> r =>
    TypeEnvKey -> Eff r (Type SourceRegion)
lookupType key = do
    -- try the local environment first to avoid unnecessary queries / infinite loops
    inEnv <- lookupTypeMaybe key
    case inEnv of
        Just ty -> pure ty
        Nothing -> do
            debug ("Type not found in environment, querying: " <> pretty key)
            Rock.fetch (Elara.Query.TypeOf key)

generateConstraints' :: (Infer SourceRegion r, Error (UnifyError SourceRegion) :> r) => ShuntedExpr -> Eff r (TypedExpr', Monotype SourceRegion)
generateConstraints' expr' =
    debugWithResult ("generateConstraints: " <> pretty expr') $
        let exprLoc = expr' ^. Syntax.exprLocation
         in case expr' ^. _Unwrapped % _1 % unlocated of
                Int i -> pure (Int i, Scalar exprLoc ScalarInt)
                Float f -> pure (Float f, Scalar exprLoc ScalarFloat)
                String s -> pure (String s, Scalar exprLoc ScalarString)
                Char c -> pure (Char c, Scalar exprLoc ScalarChar)
                Unit -> pure (Unit, Scalar exprLoc ScalarUnit)
                Constructor (Located loc name) -> do
                    -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
                    varType <- lookupType (DataConKey $ stripLocation name)

                    (instantiated, typeApps) <- instantiate varType

                    let ctor = (Constructor (Located loc name), instantiated)

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
                (Lambda (Located paramLoc (TypedLambdaParam (paramName, expectedParamType))) body) -> do
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

                    let equalityConstraint = Equality exprLoc t1 (Function e1Loc t2 (TypeVar e2Loc resultTyVar))
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

                    let recursiveConstraint = Equality exprLoc (TypeVar loc recursiveVar) varType
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

                    let equalityConstraint = Equality (cond ^. Syntax.exprLocation) condType (TypeConstructor condLoc (mkPrimQual boolName) [])
                    tell equalityConstraint

                    let equalityConstraint1 = Equality (then' ^. Syntax.exprLocation) thenType elseType
                    tell equalityConstraint1

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

                    cases' <- for cases $ \(pattern, body) -> scoped @(LocalTypeEnvironment _) $ do
                        -- Q ; Γ ⊢ p1 : τ1
                        (typedPattern, patternType) <- generatePatternConstraints pattern eType

                        -- τ1 ~ τ
                        -- let equalityConstraint = Equality (pattern ^. Syntax.patternLocation) eType patternType
                        -- tell equalityConstraint

                        -- Q ; Γ ⊢ e1 : τ2
                        (typedBody, bodyType) <- generateConstraints body

                        -- τ2 ~ τr
                        tell (Equality (body ^. Syntax.exprLocation) bodyType (TypeVar eLoc resultTyVar))

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

                    let recursiveConstraint = Equality exprLoc (TypeVar loc recursiveVar) varType
                    tell recursiveConstraint

                    modify (addLocalType varName (Lifted varType))

                    pure (Let (Located loc varName) NoFieldValue typedVarExpr, varType)

{- | Generate constraints for a pattern, returning the typed pattern and the type of the pattern, and generating constraints for the pattern
We define the type of a pattern as the type of values it can match against, rather than its "signature"

For example, in the case of a simple option type @type Option a = Some a | None@,
we say that the pattern `Some x` has type `Option a` rather than @a -> Option a@
-}
generatePatternConstraints :: (Infer SourceRegion r, Error (UnifyError SourceRegion) :> r) => ShuntedPattern -> Monotype SourceRegion -> Eff r (TypedPattern, Monotype SourceRegion)
generatePatternConstraints (Pattern (Located loc pattern', expectedType)) over = debugWithResult ("generatePatternConstraints: " <> pretty pattern') $ do
    (typedPattern', monotype) <- generatePatternConstraints' pattern' over
    tell (Equality loc monotype over)
    pure (Pattern (Located loc typedPattern', monotype), monotype)

generatePatternConstraints' ::
    (Infer SourceRegion r, Error (UnifyError SourceRegion) :> r) =>
    ShuntedPattern' ->
    {- | the type of the pattern we are matching against
    For example if we have `match (x : Option Int) with { Some y -> y }` then `over` would be `Option Int`
    This is necessary for `WildcardPattern` and `VarPattern` to know what type they should be
    -}
    Monotype SourceRegion ->
    Eff r (TypedPattern', Monotype SourceRegion)
generatePatternConstraints' pattern' over =
    debugWithResult ("generatePatternConstraints: " <> pretty pattern') $
        let patternLoc = monotypeLoc over
         in case pattern' of
                WildcardPattern -> pure (WildcardPattern, over)
                UnitPattern -> pure (UnitPattern, Scalar patternLoc ScalarUnit)
                IntegerPattern i -> pure (IntegerPattern i, Scalar patternLoc ScalarInt)
                FloatPattern f -> pure (FloatPattern f, Scalar patternLoc ScalarFloat)
                StringPattern s -> pure (StringPattern s, Scalar patternLoc ScalarString)
                CharPattern c -> pure (CharPattern c, Scalar patternLoc ScalarChar)
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
                    when (length argTys /= length args) $
                        throwError $
                            PatternConstructorArityMismatch
                                ctor
                                (length argTys)
                                (length args)
                                patternLoc
                    argPats <- for (zip args argTys) $ \(arg, argTy) -> do
                        generatePatternConstraints arg argTy

                    pure (ConstructorPattern ctor' (fmap fst argPats), res)

instantiate :: forall r loc. (loc ~ SourceRegion, Infer loc r) => Type loc -> Eff r (Monotype loc, [TypeVariable])
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
    ( Error (UnifyError a) :> r
    , StructuredDebug :> r
    , Pretty a
    , Eq a
    , Show a
    , HasCallStack
    , a ~ SourceRegion
    ) =>
    Constraint a -> Set UniqueTyVar -> Constraint a -> Eff r (Constraint a, Substitution a)
solveConstraint given tch wanted = do
    (residual, unifier) <- simplifyConstraint given tch wanted

    let residual' = reduce residual

    pure (residual', unifier)

simplifyConstraint ::
    ( Error (UnifyError loc) :> r
    , StructuredDebug :> r
    , Pretty loc
    , Eq loc
    , Show loc
    , HasCallStack
    , loc ~ SourceRegion
    ) =>
    Constraint loc -> Set UniqueTyVar -> Constraint loc -> Eff r (Constraint loc, Substitution loc)
simplifyConstraint given tch wanted = debugWithResult ("simplifyConstraint: " <> pretty (given, wanted)) $ do
    givenSubst <- reduceGiven given
    debug ("simplifyConstraint: givenSubst: " <> pretty givenSubst)
    runReader tch (solve (substituteAll givenSubst wanted))

reduceGiven :: (Pretty loc, Error (UnifyError loc) :> r, StructuredDebug :> r, Eq loc, Show loc, HasCallStack) => Constraint loc -> Eff r (Substitution loc)
reduceGiven eq@(Equality _ a b) = debugWithResult ("reduceGiven: " <> pretty eq) $ unifyGiven (Just eq) a b
reduceGiven conj@(Conjunction _ a b) = debugWithResult ("reduceGiven: " <> pretty conj) $ do
    s1 <- reduceGiven a
    s2 <- reduceGiven (substituteAll s1 b)
    pure (s1 <> s2)
reduceGiven EmptyConstraint{} = pure mempty

solve ::
    ( Error (UnifyError loc) :> r
    , StructuredDebug :> r
    , Reader (Set UniqueTyVar) :> r
    , Eq loc
    , Show loc
    , HasCallStack
    , loc ~ SourceRegion
    ) =>
    Pretty loc =>
    Constraint loc ->
    Eff r (Constraint loc, Substitution loc)
solve constraint@(Equality _ a b) = do
    let ?constraint = Just constraint in unify a b
solve (Conjunction _ a b) = do
    -- a lot of this is duplicated from unifyMany which is a bit annoying
    (c1, s1) <- solve a
    -- apply s1 to b before unifying
    (c2, s2) <- solve (substituteAll s1 b)
    pure (c1 <> c2, s1 <> s2)
solve EmptyConstraint{} = pure mempty

unifyGiven ::
    (Pretty loc, Error (UnifyError loc) :> r, StructuredDebug :> r, Show loc, Eq loc) =>
    Maybe (Constraint loc) -> Monotype loc -> Monotype loc -> Eff r (Substitution loc)
unifyGiven _ (TypeVar _ a) b = bindGiven a b
unifyGiven _ a (TypeVar _ b) = bindGiven b a
unifyGiven c (TypeConstructor _ a as) (TypeConstructor _ b bs)
    | a /= b = throwError TypeConstructorMismatch
    | length as /= length bs = throwError ArityMismatch
    | otherwise = unifyGivenMany c as bs
unifyGiven constraint (Function _ a b) (Function _ c d) = unifyGivenMany constraint [a, b] [c, d]
unifyGiven _ (Scalar _ a) (Scalar _ b) =
    if a == b
        then pure mempty
        else throwError ScalarMismatch
unifyGiven constraint a b = throwError $ UnificationFailed constraint (a, b)

unifyGivenMany constraint xs ys = foldrM go mempty (zip xs ys)
  where
    go (a, b) s = do
        s' <- unifyGiven constraint (substituteAll s a) (substituteAll s b)
        pure (s <> s')

unify ::
    forall loc r.
    ( HasCallStack
    , StructuredDebug :> r
    , Error (UnifyError loc) :> r
    , Reader (Set UniqueTyVar) :> r
    , Pretty loc
    , Pretty (Constraint loc)
    , Eq loc
    , Show loc
    , loc ~ SourceRegion
    , ?constraint :: Maybe (Constraint loc)
    ) =>
    Monotype loc ->
    Monotype loc ->
    Eff r (Constraint loc, Substitution loc)
unify a b = do
    unify' a b
  where
    unify' ::
        Monotype loc -> Monotype loc -> Eff r (Constraint loc, Substitution loc)

    unify' (TypeVar _ a) (TypeVar _ b) | a == b = pure (mempty, mempty)
    unify' (TypeVar _ (UnificationVar a)) b = unifyVar a b
    unify' a (TypeVar _ (UnificationVar b)) = unifyVar b a
    unify' (Scalar _ a) (Scalar _ b) =
        if a == b
            then pure mempty
            else throwError ScalarMismatch
    unify' (TypeConstructor _ a as) (TypeConstructor _ b bs)
        | a /= b = throwError TypeConstructorMismatch
        | length as /= length bs = throwError ArityMismatch
        | otherwise = unifyMany as bs
    unify' (Function _ a b) (Function _ c d) = unifyMany [a, b] [c, d]
    unify' a b = throwError $ UnificationFailed ?constraint (a, b)

bindGiven ::
    (StructuredDebug :> r, Error (UnifyError loc) :> r, Show loc) =>
    TypeVariable -> Monotype loc -> Eff r (Substitution loc)
bindGiven a t =
    if member a (ftv t)
        then throwError (OccursCheckFailed a t)
        else pure (substitution (tvValue a, t))
  where
    tvValue (UnificationVar a) = a
    tvValue (SkolemVar a) = a

unifyVar ::
    forall loc r.
    (StructuredDebug :> r, Error (UnifyError loc) :> r, Reader (Set UniqueTyVar) :> r, Show loc, Eq loc) =>
    UniqueTyVar -> Monotype loc -> Eff r (Constraint loc, Substitution loc)
unifyVar a t = do
    debug $ "bind " <> pretty a <> " to " <> pretty t
    bindVar a t
  where
    bindVar :: UniqueTyVar -> Monotype loc -> Eff r (Constraint loc, Substitution loc)
    bindVar tv t | member tv (fuv t) = throwError (OccursCheckFailed (UnificationVar tv) t)
    bindVar tv t = do
        tch <- ask
        debug ("bindVar " <> pretty tv <> " to " <> pretty t)
        if member tv tch
            then pure (EmptyConstraint (monotypeLoc t), substitution (tv, t))
            else pure (Equality (monotypeLoc t) (TypeVar (monotypeLoc t) $ UnificationVar tv) t, mempty)

unifyMany ::
    ( HasCallStack
    , StructuredDebug :> r
    , Pretty loc
    , Reader (Set UniqueTyVar) :> r
    , Eq loc
    , Show loc
    , Semigroup loc
    , loc ~ SourceRegion
    , ?constraint :: Maybe (Constraint loc)
    ) =>
    Error (UnifyError loc) :> r =>
    [Monotype loc] ->
    [Monotype loc] ->
    Eff r (Constraint loc, Substitution loc)
unifyMany [] [] = pure (mempty, mempty)
unifyMany [] _ = throwError UnifyMismatch
unifyMany _ [] = throwError UnifyMismatch
unifyMany (a : as) (b : bs) = do
    (c1, s1) <- unify a b
    (c2, s2) <- unifyMany (fmap (substituteAll s1) as) (fmap (substituteAll s1) bs)
    pure (c1 <> c2, s1 <> s2)

data UnifyError loc
    = OccursCheckFailed TypeVariable (Monotype loc)
    | ScalarMismatch
    | TypeConstructorMismatch
    | ArityMismatch
    | UnificationFailed (Maybe (Constraint loc)) (Monotype loc, Monotype loc)
    | UnifyMismatch
    | UnresolvedConstraint (Qualified VarName) (Constraint SourceRegion)
    | PatternConstructorArityMismatch DataCon Int Int loc
    deriving (Generic, Show)

instance Pretty loc => Pretty (UnifyError loc)

instance ReportableError (UnifyError SourceRegion) where
    report (OccursCheckFailed a t) =
        writeReport $
            Err
                Nothing
                ("Occurs check failed: " <> pretty a <> " in " <> pretty t)
                []
                []
    report (UnificationFailed constraint (a, b)) = do
        let aRegion = sourceRegionToDiagnosePosition $ monotypeLoc a
        let bRegion = sourceRegionToDiagnosePosition $ monotypeLoc b
        let constraints = fmap (universeOf plate) constraint ?: []
        writeReport $
            Err
                Nothing
                ("Could not unify types: " <> pretty a <> " and " <> pretty b)
                ( [ (aRegion, This (pretty a))
                  , (bRegion, This (pretty b))
                  ]
                    ++ fmap (\c -> (sourceRegionToDiagnosePosition (constraintLoc c), This ("Constraint: " <> pretty c))) constraints
                )
                []
    report y = defaultReport y
