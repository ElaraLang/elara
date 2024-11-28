{-# LANGUAGE NoPatternSynonyms #-}

module Elara.TypeInfer.ConstraintGeneration where

import Elara.AST.Generic (Expr (..), Expr' (..))
import Elara.AST.Generic.Common (NoFieldValue (NoFieldValue))
import Elara.AST.Generic.Types (Pattern (..), Pattern' (..), TypedLambdaParam (..))
import Elara.AST.Generic.Types qualified as Syntax
import Elara.AST.Name (VarName (..))
import Elara.AST.Region (Located (Located), SourceRegion)
import Elara.AST.Shunted (ShuntedExpr, ShuntedExpr', ShuntedPattern, ShuntedPattern')
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.AST.Typed (TypedExpr, TypedExpr', TypedPattern, TypedPattern')
import Elara.AST.VarRef
import Elara.Data.Pretty
import Elara.Data.Unique (UniqueGen, uniqueGenToIO)
import Elara.Error (ReportableError (..), runErrorOrReport, writeReport)
import Elara.Logging (StructuredDebug, debug, debugWith, structuredDebugToLog)
import Elara.Pipeline
import Elara.TypeInfer.Environment (InferError, LocalTypeEnvironment, TypeEnvKey (..), TypeEnvironment, addLocalType, emptyLocalTypeEnvironment, emptyTypeEnvironment, lookupLocalVar, lookupType, withLocalType)
import Elara.TypeInfer.Ftv (occurs)
import Elara.TypeInfer.Type (AxiomScheme, Constraint (..), Monotype (..), Scalar (..), Substitutable (..), Substitution (..), Type (..), substitution)
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)
import Error.Diagnose
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.State.Extra (scoped)
import Polysemy.Writer
import Print (debugPretty)
import TODO (todo)

type InferEffects loc =
    '[ Writer (Constraint loc)
     , State (LocalTypeEnvironment loc)
     , State (TypeEnvironment loc)
     , Error (InferError loc)
     , UniqueGen
     , StructuredDebug
     ]
type Infer loc r = Members (InferEffects loc) r

runInferEffects :: forall r a loc. IsPipeline r => Sem (EffectsAsPrefixOf (InferEffects loc) r) a -> Sem r (Constraint loc, a)
runInferEffects e = do
    e
        & uniqueGenToIO
        . runErrorOrReport
        . evalState emptyLocalTypeEnvironment
        . evalState emptyTypeEnvironment
        . runWriter
        . structuredDebugToLog
        . subsume_

generateConstraints :: Infer SourceRegion r => ShuntedExpr -> Sem r (TypedExpr, Monotype SourceRegion)
generateConstraints (Expr (Located loc expr', expectedType)) = do
    (typedExpr', monotype) <- generateConstraints' expr'
    pure (Expr (Located loc typedExpr', monotype), monotype)

generateConstraints' :: Infer SourceRegion r => ShuntedExpr' -> Sem r (TypedExpr', Monotype SourceRegion)
generateConstraints' expr' = debugWith ("generateConstraints: " <> pretty expr') $ do
    case expr' of
        Int i -> pure (Int i, Scalar ScalarInt)
        Float f -> pure (Float f, Scalar ScalarFloat)
        String s -> pure (String s, Scalar ScalarString)
        Char c -> pure (Char c, Scalar ScalarChar)
        Unit -> pure (Unit, Scalar ScalarUnit)
        Constructor (Located loc name) -> do
            -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
            varType <- lookupType (DataConKey $ stripLocation name)
            case varType of
                Lifted monotype -> pure (Constructor (Located loc name), monotype)
                (Forall tyVars constraint monotype) -> do
                    -- tv
                    fresh <- makeUniqueTyVar -- make a fresh type variable for the type of the variable
                    let
                        -- Q1[α/tv]
                        instantiatedConstraint =
                            foldr (\tyVar -> substitute tyVar (TypeVar fresh)) constraint tyVars
                        -- τ1[α/tv]
                        instantiatedMonotype =
                            foldr (\tyVar -> substitute tyVar (TypeVar fresh)) monotype tyVars

                    -- τ ~ τ1[α/tv]
                    let equalityConstraint = Equality monotype instantiatedMonotype

                    tell (instantiatedConstraint <> equalityConstraint)

                    pure (Constructor (Located loc name), instantiatedMonotype)

        -- VAR
        -- local variables
        Var v'@(Located _ (Local (Located _ v))) -> do
            local <- lookupLocalVar v
            fresh <- makeUniqueTyVar
            let constraint = Equality local (TypeVar fresh)
            tell constraint
            debug ("generateConstraints: " <> pretty v <> " has type " <> pretty local <> ", creating constraint " <> pretty constraint)
            pure (Var v', TypeVar fresh)
        -- global variables
        Var (Located l vr@(Global (Located _ v))) -> do
            -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
            varType <- lookupType (TermVarKey $ stripLocation v)
            case varType of
                Lifted monotype -> pure (Var (Located l vr), monotype)
                (Forall tyVars constraint monotype) -> do
                    -- tv
                    fresh <- makeUniqueTyVar -- make a fresh type variable for the type of the variable
                    let
                        -- Q1[α/tv]
                        instantiatedConstraint =
                            foldr (\tyVar -> substitute tyVar (TypeVar fresh)) constraint tyVars
                        -- τ1[α/tv]
                        instantiatedMonotype =
                            foldr (\tyVar -> substitute tyVar (TypeVar fresh)) monotype tyVars

                    -- τ ~ τ1[α/tv]
                    let equalityConstraint = Equality monotype instantiatedMonotype

                    tell (instantiatedConstraint <> equalityConstraint)

                    pure (Var (Located l vr), instantiatedMonotype)

        -- ABS
        (Lambda (Located paramLoc (TypedLambdaParam (paramName, expectedParamType))) body) -> do
            paramTyVar <- makeUniqueTyVar

            (typedBody, bodyType) <- withLocalType paramName (TypeVar paramTyVar) $ do
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

            resultTyVar <- makeUniqueTyVar

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
            recursiveVar <- makeUniqueTyVar
            (typedVarExpr, varType) <-
                withLocalType varName (TypeVar recursiveVar) $
                    generateConstraints varExpr

            let recursiveConstraint = Equality (TypeVar recursiveVar) varType
            tell recursiveConstraint

            (typedBody, bodyType) <-
                withLocalType varName (varType) $
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
            resultTyVar <- makeUniqueTyVar

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
            recursiveVar <- makeUniqueTyVar
            (typedVarExpr, varType) <- withLocalType varName (TypeVar recursiveVar) $ do
                generateConstraints varExpr

            let recursiveConstraint = Equality (TypeVar recursiveVar) varType
            tell recursiveConstraint

            modify (addLocalType varName varType)

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
            varType <- makeUniqueTyVar
            modify (addLocalType (NormalVarName <$> varName) (TypeVar varType))

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

            res <- makeUniqueTyVar

            let argsFunction = foldr Function (TypeVar res) (fmap snd args')

            case t of
                Lifted monoCtor -> do
                    tell (Equality monoCtor argsFunction)

                    pure (ConstructorPattern ctor' (fmap fst args'), monoCtor)
                Forall tyVars constraint monoCtor -> do
                    fresh <- makeUniqueTyVar

                    let
                        instantiatedConstraint =
                            foldr (\tyVar -> substitute tyVar (TypeVar fresh)) constraint tyVars
                        instantiatedMonotype =
                            foldr (\tyVar -> substitute tyVar (TypeVar fresh)) monoCtor tyVars

                    tell (instantiatedConstraint <> Equality instantiatedMonotype argsFunction)

                    pure (ConstructorPattern ctor' (fmap fst args'), instantiatedMonotype)

solveConstraints :: (Member (Error (UnifyError loc)) r, Member StructuredDebug r) => Pretty loc => AxiomScheme loc -> Constraint loc -> Constraint loc -> Sem r (Constraint loc, Substitution loc)
solveConstraints axioms given wanted = do
    (substitution, simplifiedWanted) <- unifyEquality wanted

    -- let simplifiedGiven = substitute substitution given

    -- let simplifiedWanted' = substitute substitution simplifiedWanted

    -- let (entailable, residualWanted) = entail axioms simplifiedGiven simplifiedWanted'

    pure (todo, substitution)

unifyEquality :: (Member (Error (UnifyError loc)) r, Member StructuredDebug r) => Pretty loc => Constraint loc -> Sem r (Substitution loc, Constraint loc)
unifyEquality (Equality a b) = unify a b
unifyEquality (Conjunction a b) = debugWith ("unifyEquality: " <> pretty a <> " ^ " <> pretty b) $ do
    -- a lot of this is duplicated from unifyMany which is a bit annoying
    (s1, c1) <- unifyEquality a
    debug ("unifyEquality: " <> pretty s1 <> ", " <> pretty c1)
    -- apply s1 to b before unifying
    (s2, c2) <- unifyEquality (substituteAll s1 b)
    pure (s1 <> s2, c1 <> c2)
unifyEquality EmptyConstraint = pure (mempty, EmptyConstraint)

unify ::
    HasCallStack =>
    Member StructuredDebug r =>
    Pretty (Constraint loc) =>
    Member (Error (UnifyError loc)) r =>
    Monotype loc -> Monotype loc -> Sem r (Substitution loc, Constraint loc)
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
        Monotype loc -> Monotype loc -> Sem r (Substitution loc, Constraint loc)
    unify' (TypeVar a) b = (,EmptyConstraint) <$> bind a b
    unify' a (TypeVar b) = (,EmptyConstraint) <$> bind b a
    unify' (Scalar a) (Scalar b) =
        if a == b
            then pure (mempty, EmptyConstraint)
            else throw ScalarMismatch
    unify' (TypeConstructor a as) (TypeConstructor b bs)
        | a /= b = throw TypeConstructorMismatch
        | length as /= length bs = throw ArityMismatch
        | otherwise = unifyMany as bs
    unify' (Function a b) (Function c d) = unifyMany [a, b] [c, d]
    unify' a b = throw $ UnificationFailed $ "Unification failed: " <> show a <> " and " <> show b

bind ::
    (Member StructuredDebug r, Member (Error (UnifyError loc)) r) =>
    UniqueTyVar -> Monotype loc -> Sem r (Substitution loc)
bind a t = do
    debug $ "bind " <> pretty a <> " to " <> pretty t
    bind' a t
  where
    bind' a t | t == TypeVar a = pure mempty
    bind' a t | occurs a t = throw (OccursCheckFailed a t)
    bind' a t = pure $ substitution (a, t)

unifyMany ::
    (HasCallStack, Member StructuredDebug r) =>
    Member (Error (UnifyError loc)) r =>
    Pretty (Constraint loc) =>
    [Monotype loc] ->
    [Monotype loc] ->
    Sem r (Substitution loc, Constraint loc)
unifyMany [] [] = pure (mempty, EmptyConstraint)
unifyMany [] _ = throw UnifyMismatch
unifyMany _ [] = throw UnifyMismatch
unifyMany (a : as) (b : bs) = debugWith ("unifyMany: " <> pretty a <> " with" <> pretty b) $ do
    (s1 :: Substitution loc, c1) <- unify a b
    (s2, c2) <- unifyMany (fmap (substituteAll s1) as) (fmap (substituteAll s1) bs)
    pure (s1 <> s2, c1 <> c2)

data UnifyError loc
    = HasCallStack => OccursCheckFailed UniqueTyVar (Monotype loc)
    | ScalarMismatch
    | TypeConstructorMismatch
    | ArityMismatch
    | UnificationFailed String
    | UnifyMismatch
    | UnresolvedConstraint (Constraint SourceRegion)

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
