{-# LANGUAGE NoPatternSynonyms #-}

module Elara.TypeInfer.ConstraintGeneration where

import Data.Foldable (foldrM)
import Data.Generics.Product (HasAny (the), HasType (typed))
import Data.Generics.Wrapped (_Unwrapped)
import Data.Set (isSubsetOf, member)
import Elara.AST.Generic (Expr (..), Expr' (..))
import Elara.AST.Generic.Common (NoFieldValue (NoFieldValue))
import Elara.AST.Generic.Types (Pattern (..), Pattern' (..), TypedLambdaParam (..))
import Elara.AST.Generic.Types qualified as Syntax
import Elara.AST.Name (Qualified, VarName (..))
import Elara.AST.Region (Located (Located), SourceRegion)
import Elara.AST.Shunted (ShuntedExpr, ShuntedExpr', ShuntedPattern, ShuntedPattern')
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.AST.Typed (TypedExpr, TypedExpr', TypedPattern, TypedPattern')
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Data.Unique (uniqueGenToIO)
import Elara.Error (ReportableError (..), runErrorOrReport, writeReport)
import Elara.Logging (StructuredDebug, debug, debugWith, debugWithResult)
import Elara.Pipeline
import Elara.TypeInfer.Environment (LocalTypeEnvironment, TypeEnvKey (..), addLocalType, emptyLocalTypeEnvironment, emptyTypeEnvironment, lookupLocalVar, lookupType, withLocalType)
import Elara.TypeInfer.Ftv (Ftv (..), Fuv (fuv), occurs)
import Elara.TypeInfer.Generalise (generalise)
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type (AxiomScheme, Constraint (..), Monotype (..), Polytype (..), Scalar (..), Substitutable (..), Substitution (..), Type (..), TypeVariable (SkolemVar, UnificationVar), reduce, substitution)
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)
import Error.Diagnose
import Polysemy
import Polysemy.Error
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State
import Polysemy.State.Extra (scoped)
import Polysemy.Writer
import Print (debugPretty, showPretty)

runInferEffects :: forall r a loc. IsPipeline r => Sem (EffectsAsPrefixOf (InferEffects loc) r) a -> Sem r (Constraint loc, a)
runInferEffects e = do
    e
        & uniqueGenToIO
        . runErrorOrReport
        . evalState emptyLocalTypeEnvironment
        . evalState emptyTypeEnvironment
        . runWriter
        . subsume_

generateConstraints :: Infer SourceRegion r => ShuntedExpr -> Sem r (TypedExpr, Monotype SourceRegion)
generateConstraints (Expr (Located loc expr', expectedType)) = do
    (typedExpr', monotype) <- generateConstraints' expr'
    pure (Expr (Located loc typedExpr', monotype), monotype)

generateConstraints' :: Infer SourceRegion r => ShuntedExpr' -> Sem r (TypedExpr', Monotype SourceRegion)
generateConstraints' expr' = debugWithResult ("generateConstraints: " <> pretty expr') $ do
    case expr' of
        Int i -> pure (Int i, Scalar ScalarInt)
        Float f -> pure (Float f, Scalar ScalarFloat)
        String s -> pure (String s, Scalar ScalarString)
        Char c -> pure (Char c, Scalar ScalarChar)
        Unit -> pure (Unit, Scalar ScalarUnit)
        Constructor (Located loc name) -> do
            -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
            varType <- lookupType (DataConKey $ stripLocation name)

            instantiated <- instantiate varType

            pure (Constructor (Located loc name), instantiated)

        -- VAR
        Var v'@(Located _ varName) -> do
            varType <- case varName of
                Local (Located _ n) -> lookupLocalVar n
                Global (Located _ n) -> lookupType (TermVarKey n)
            -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
            instantiated <- instantiate varType

            pure (Var v', instantiated)
        -- ABS
        (Lambda (Located paramLoc (TypedLambdaParam (paramName, expectedParamType))) body) -> do
            paramTyVar <- UnificationVar <$> makeUniqueTyVar

            (typedBody, bodyType) <- withLocalType paramName (Lifted $ TypeVar paramTyVar) $ do
                generateConstraints body

            let functionType = (TypeVar paramTyVar) `Function` bodyType

            pure
                ( Lambda (Located paramLoc (TypedLambdaParam (paramName, (TypeVar paramTyVar)))) typedBody
                , functionType
                )

        -- APP
        FunctionCall e1 e2 -> do
            (e1', t1) <- generateConstraints e1
            (e2', t2) <- generateConstraints e2

            resultTyVar <- UnificationVar <$> makeUniqueTyVar

            let equalityConstraint = Equality t1 (Function t2 (TypeVar resultTyVar))
            debug (pretty equalityConstraint)
            tell equalityConstraint

            pure (FunctionCall e1' e2', TypeVar resultTyVar)

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
                withLocalType varName (Lifted $ TypeVar recursiveVar) $
                    generateConstraints varExpr

            let recursiveConstraint = Equality (TypeVar recursiveVar) varType
            tell recursiveConstraint

            -- TODO: we need to check if e1 is closed here before generalising _everything_

            env <- get
            let freeVarsInExpr = ftv varType
                freeVarsInEnv = ftv env

            debug (pretty freeVarsInExpr <> " vs " <> pretty freeVarsInEnv)
            maybeGeneralised <-
                if True
                    then do
                        generalised <- generalise varType
                        debug (pretty varType <> " -> generalised: " <> pretty generalised)
                        pure (Polytype generalised)
                    else pure (Lifted varType)

            (typedBody, bodyType) <-
                withLocalType varName (maybeGeneralised) $
                    generateConstraints body

            pure (LetIn (Located loc varName) NoFieldValue typedVarExpr typedBody, bodyType)

        -- IF
        If cond then' else' -> do
            (typedCond, condType) <- generateConstraints cond
            (typedThen, thenType) <- generateConstraints then'
            (typedElse, elseType) <- generateConstraints else'

            let equalityConstraint = Equality condType (Scalar ScalarBool)
            tell equalityConstraint

            let equalityConstraint1 = Equality thenType elseType
            tell equalityConstraint1

            pure (If typedCond typedThen typedElse, thenType)
        TypeApplication e ty -> do
            error "i dont know what to do with type applications yet sorry"

        -- MATCH
        -- (match (e: τ) with { p1 -> e1; ...; pn -> en }) : τr
        Match e cases -> do
            -- Q ; Γ ⊢ e : τ
            (typedE, eType) <- generateConstraints e

            -- τr
            resultTyVar <- UnificationVar <$> makeUniqueTyVar

            cases' <- for cases $ \(pattern, body) -> scoped @(LocalTypeEnvironment _) $ do
                -- Q ; Γ ⊢ p1 : τ1
                (typedPattern, patternType) <- generatePatternConstraints pattern

                -- τ1 ~ τ
                let equalityConstraint = Equality eType patternType
                tell equalityConstraint

                -- Q ; Γ ⊢ e1 : τ2
                (typedBody, bodyType) <- generateConstraints body

                -- τ2 ~ τr
                tell (Equality bodyType (TypeVar resultTyVar))

                pure (typedPattern, typedBody)

            pure (Match typedE cases', TypeVar resultTyVar)
        Block exprs -> do
            vals <- for exprs $ \expr -> do
                generateConstraints expr

            let exprs = fmap fst vals

            let exprTypes = fmap snd vals

            pure $ ((Syntax.Block exprs), last exprTypes)
        Let (Located loc varName) NoFieldValue varExpr -> do
            recursiveVar <- UnificationVar <$> makeUniqueTyVar
            (typedVarExpr, varType) <- withLocalType varName (Lifted $ TypeVar recursiveVar) $ do
                generateConstraints varExpr

            let recursiveConstraint = Equality (TypeVar recursiveVar) varType
            tell recursiveConstraint

            modify (addLocalType varName (Lifted varType))

            pure (Let (Located loc varName) NoFieldValue typedVarExpr, varType)

generatePatternConstraints :: Infer SourceRegion r => ShuntedPattern -> Sem r (TypedPattern, Monotype SourceRegion)
generatePatternConstraints (Pattern (Located loc pattern', expectedType)) = do
    (typedPattern', monotype) <- generatePatternConstraints' pattern'
    pure (Pattern (Located loc typedPattern', monotype), monotype)

generatePatternConstraints' :: Infer SourceRegion r => ShuntedPattern' -> Sem r (TypedPattern', Monotype SourceRegion)
generatePatternConstraints' pattern' =
    case pattern' of
        WildcardPattern -> pure (WildcardPattern, Scalar ScalarUnit)
        UnitPattern -> pure (UnitPattern, Scalar ScalarUnit)
        IntegerPattern i -> pure (IntegerPattern i, Scalar ScalarInt)
        FloatPattern f -> pure (FloatPattern f, Scalar ScalarFloat)
        StringPattern s -> pure (StringPattern s, Scalar ScalarString)
        CharPattern c -> pure (CharPattern c, Scalar ScalarChar)
        VarPattern (Located loc varName) -> do
            varType <- UnificationVar <$> makeUniqueTyVar
            modify (addLocalType (NormalVarName <$> varName) (Lifted $ TypeVar varType))

            pure (VarPattern (Located loc (NormalVarName <$> varName)), TypeVar varType)
        ConstructorPattern ctor'@(Located _ ctor) args -> do
            t <- lookupType (DataConKey ctor)

            -- if we have a constructor @Ctor : x -> y -> Z@
            -- and pattern @Ctor a b@
            -- we need to generate @a : x@ and @b : y@
            -- we do this by making one big constraint out of fresh type variables @a_1 -> @b_1 -> ... -> Z@
            -- and emitting a single equality constraint @x -> y -> Z = a_1 -> b_1 -> ... -> Z@

            args' <- for args $ \arg -> do
                generatePatternConstraints arg

            res <- UnificationVar <$> makeUniqueTyVar

            let argsFunction = foldr Function (TypeVar res) (fmap snd args')

            case t of
                Lifted monoCtor -> do
                    tell (Equality monoCtor argsFunction)

                    pure (ConstructorPattern ctor' (fmap fst args'), monoCtor)
                Polytype (Forall tyVars constraint monoCtor) -> do
                    fresh <- UnificationVar <$> makeUniqueTyVar

                    let
                        instantiatedConstraint =
                            foldr (\tyVar -> substitute (view typed tyVar) (TypeVar fresh)) constraint tyVars
                        instantiatedMonotype =
                            foldr (\tyVar -> substitute (view typed tyVar) (TypeVar fresh)) monoCtor tyVars

                    tell (instantiatedConstraint <> Equality instantiatedMonotype argsFunction)

                    pure (ConstructorPattern ctor' (fmap fst args'), instantiatedMonotype)

instantiate :: forall r loc. Infer loc r => Type loc -> Sem r (Monotype loc)
instantiate (Lifted t) = pure t
instantiate (Polytype (Forall tyVars constraint t)) = debugWith ("instantiate: " <> pretty t) $ do
    fresh <- mapM (const (UnificationVar <$> makeUniqueTyVar)) tyVars
    let substitution = Substitution $ fromList $ zip (fmap (view typed) tyVars) (fmap TypeVar fresh)
    let
        instantiatedConstraint =
            substituteAll substitution constraint
        instantiatedMonotype =
            substituteAll substitution t

    tell instantiatedConstraint
    pure instantiatedMonotype

solveConstraint :: (Member (Error (UnifyError a)) r, Member StructuredDebug r, Pretty a) => Constraint a -> Set UniqueTyVar -> Constraint a -> Sem r (Constraint a, Substitution a)
solveConstraint given tch wanted = debugWith ("solveConstraint: given: " <> pretty given <> " wanted: " <> pretty wanted) $ do
    (residual, unifier) <- simplifyConstraint given tch wanted
    debug ("solveConstraint: " <> pretty residual)
    let residual' = reduce residual
    debug ("solveConstraint reduced: " <> pretty residual')

    debug ("solveConstraint: unifier: " <> pretty unifier)
    pure (residual', unifier)

simplifyConstraint ::
    ( Member (Error (UnifyError loc)) r
    , Member StructuredDebug r
    , Pretty loc
    ) =>
    Constraint loc -> Set UniqueTyVar -> Constraint loc -> Sem r (Constraint loc, Substitution loc)
simplifyConstraint given tch wanted = do
    givenSubst <- (reduceGiven given)
    debug ("simplifyConstraint: givenSubst: " <> pretty givenSubst)
    runReader tch (solve (substituteAll givenSubst wanted))

reduceGiven :: (Pretty loc, Member (Error (UnifyError loc)) r, Member StructuredDebug r) => Constraint loc -> Sem r (Substitution loc)
reduceGiven (Equality a b) = unifyGiven a b
reduceGiven (Conjunction a b) = do
    s1 <- reduceGiven a
    s2 <- reduceGiven (substituteAll s1 b)
    pure (s1 <> s2)
reduceGiven EmptyConstraint = pure mempty

solve ::
    (Member (Error (UnifyError loc)) r, Member StructuredDebug r, Member (Reader (Set UniqueTyVar)) r) =>
    Pretty loc =>
    Constraint loc ->
    Sem r (Constraint loc, Substitution loc)
solve (Equality a b) = unify a b
solve (Conjunction a b) = debugWith ("solve: " <> pretty a <> " ^ " <> pretty b) $ do
    -- a lot of this is duplicated from unifyMany which is a bit annoying
    (c1, s1) <- solve a
    debug ("solve produced: " <> pretty s1)
    -- apply s1 to b before unifying
    (c2, s2) <- solve (substituteAll s1 b)
    pure (c1 <> c2, s1 <> s2)
solve EmptyConstraint = pure (mempty)

unifyGiven ::
    (Pretty loc, Member (Error (UnifyError loc)) r, Member StructuredDebug r) =>
    Monotype loc -> Monotype loc -> Sem r (Substitution loc)
unifyGiven (TypeVar a) b = bindGiven a b
unifyGiven a (TypeVar b) = bindGiven b a
unifyGiven (TypeConstructor a as) (TypeConstructor b bs)
    | a /= b = throw TypeConstructorMismatch
    | length as /= length bs = throw ArityMismatch
    | otherwise = unifyGivenMany as bs
unifyGiven (Function a b) (Function c d) = unifyGivenMany [a, b] [c, d]
unifyGiven (Scalar a) (Scalar b) =
    if a == b
        then pure (mempty)
        else throw ScalarMismatch
unifyGiven a b = throw $ UnificationFailed $ "Unification failed: " <> show a <> " and " <> show b

unifyGivenMany xs ys = foldrM go mempty (zip xs ys)
  where
    go (a, b) s = do
        s' <- unifyGiven (substituteAll s a) (substituteAll s b)
        pure (s <> s')

unify ::
    HasCallStack =>
    Member StructuredDebug r =>
    Pretty (Constraint loc) =>
    Member (Error (UnifyError loc)) r =>
    Member (Reader (Set UniqueTyVar)) r =>
    Pretty loc =>
    Monotype loc -> Monotype loc -> Sem r (Constraint loc, Substitution loc)
unify a b = debugWith ("unify " <> pretty a <> " with " <> pretty b) $ do
    r <- unify' a b
    debug ("unify result: " <> pretty r)
    pure r
  where
    unify' ::
        HasCallStack =>
        Member StructuredDebug r =>
        Member (Error (UnifyError loc)) r =>
        Pretty (Constraint loc) =>
        Member (Reader (Set UniqueTyVar)) r =>
        Pretty loc =>
        Monotype loc -> Monotype loc -> Sem r (Constraint loc, Substitution loc)

    unify' (TypeVar a) (TypeVar b) | a == b = pure (mempty, mempty)
    unify' (TypeVar (UnificationVar a)) b = unifyVar a b
    unify' a (TypeVar (UnificationVar b)) = unifyVar b a
    unify' (Scalar a) (Scalar b) =
        if a == b
            then pure (mempty)
            else throw ScalarMismatch
    unify' (TypeConstructor a as) (TypeConstructor b bs)
        | a /= b = throw TypeConstructorMismatch
        | length as /= length bs = throw ArityMismatch
        | otherwise = unifyMany as bs
    unify' (Function a b) (Function c d) = unifyMany [a, b] [c, d]
    unify' a b = throw $ UnificationFailed $ "Unification failed: " <> show a <> " and " <> show b

bindGiven ::
    (Member StructuredDebug r, Member (Error (UnifyError loc)) r) =>
    TypeVariable -> Monotype loc -> Sem r (Substitution loc)
bindGiven a t =
    if member a (ftv t)
        then throw (OccursCheckFailed a t)
        else pure (substitution (tvValue a, t))
  where
    tvValue (UnificationVar a) = a
    tvValue (SkolemVar a) = a

unifyVar ::
    forall loc r.
    (Member StructuredDebug r, Member (Error (UnifyError loc)) r, Member (Reader (Set UniqueTyVar)) r) =>
    UniqueTyVar -> Monotype loc -> Sem r (Constraint loc, Substitution loc)
unifyVar a t = do
    debug $ "bind " <> pretty a <> " to " <> pretty t
    bindVar a t
  where
    bindVar :: UniqueTyVar -> Monotype loc -> Sem r (Constraint loc, Substitution loc)
    bindVar tv t | member tv (fuv t) = throw (OccursCheckFailed (UnificationVar tv) t)
    bindVar tv t = do
        tch <- ask
        debug ("bindVar " <> pretty tv <> " to " <> pretty t <> " with " <> pretty tch)
        if member tv tch
            then pure (mempty, substitution (tv, t))
            else pure (Equality (TypeVar $ UnificationVar tv) t, mempty)

unifyMany ::
    (HasCallStack, Member StructuredDebug r, Pretty loc, Member (Reader (Set UniqueTyVar)) r) =>
    Member (Error (UnifyError loc)) r =>
    [Monotype loc] ->
    [Monotype loc] ->
    Sem r (Constraint loc, Substitution loc)
unifyMany [] [] = pure (mempty, mempty)
unifyMany [] _ = throw UnifyMismatch
unifyMany _ [] = throw UnifyMismatch
unifyMany (a : as) (b : bs) = debugWith ("unifyMany: " <> pretty a <> " with " <> pretty b) $ do
    (c1, s1) <- unify a b
    debug ("unifyMany produced: " <> pretty (c1, s1))
    debug ("unifyMany: as: " <> pretty as <> " bs: " <> pretty bs)
    (c2, s2) <- unifyMany (fmap (substituteAll s1) as) (fmap (substituteAll s1) bs)
    pure (c1 <> c2, s1 <> s2)

data UnifyError loc
    = HasCallStack => OccursCheckFailed TypeVariable (Monotype loc)
    | ScalarMismatch
    | TypeConstructorMismatch
    | ArityMismatch
    | UnificationFailed Text
    | UnifyMismatch
    | UnresolvedConstraint (Qualified VarName) (Constraint SourceRegion)

deriving instance Show (UnifyError loc)

instance ReportableError (UnifyError loc) where
    report (OccursCheckFailed a t) =
        writeReport $
            Err
                (Nothing)
                ("Occurs check failed: " <> pretty a <> " in " <> pretty t)
                []
                []
    report y = writeReport $ Err (Nothing) (show y) [] []
