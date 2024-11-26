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
import Elara.Data.Unique (UniqueGen)
import Elara.TypeInfer.Environment (InferError, LocalTypeEnvironment, TypeEnvKey (..), TypeEnvironment, addLocalType, lookupLocalVar, lookupType, withLocalType)
import Elara.TypeInfer.Ftv (occurs)
import Elara.TypeInfer.Type (AxiomScheme, Constraint (..), Monotype (..), Scalar (..), Substitutable (..), Substitution (..), Type (..), substitution)
import Elara.TypeInfer.Unique (UniqueTyVar, makeUniqueTyVar)
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.State.Extra (scoped)
import Polysemy.Writer
import Print (debugPretty)
import TODO (todo)

type InferEffects loc = '[Writer (Constraint loc), State (LocalTypeEnvironment loc), Error (InferError loc), UniqueGen]
type Infer loc r = Members (InferEffects loc) r

generateConstraints :: Infer SourceRegion r => TypeEnvironment SourceRegion -> ShuntedExpr -> Sem r (TypedExpr, Monotype SourceRegion)
generateConstraints env (Expr (Located loc expr', expectedType)) = do
    (typedExpr', monotype) <- generateConstraints' env expr'
    pure (Expr (Located loc typedExpr', monotype), monotype)

generateConstraints' :: Infer SourceRegion r => TypeEnvironment SourceRegion -> ShuntedExpr' -> Sem r (TypedExpr', Monotype SourceRegion)
generateConstraints' env expr' =
    case expr' of
        Int i -> pure (Int i, Scalar ScalarInt)
        Float f -> pure (Float f, Scalar ScalarFloat)
        String s -> pure (String s, Scalar ScalarString)
        Char c -> pure (Char c, Scalar ScalarChar)
        Unit -> pure (Unit, Scalar ScalarUnit)
        Constructor (Located loc name) -> do
            -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
            varType <- lookupType (DataConKey $ stripLocation name) env
            case varType of
                Lifted monotype -> pure (Constructor (Located loc name), monotype)
                (Forall tyVar constraint monotype) -> do
                    debugPretty ("Constraint for " <> pretty name <> "= " <> pretty constraint)
                    -- tv
                    fresh <- makeUniqueTyVar -- make a fresh type variable for the type of the variable
                    let
                        -- Q1[α/tv]
                        instantiatedConstraint =
                            substitute tyVar (TypeVar fresh) constraint
                        -- τ1[α/tv]
                        instantiatedMonotype =
                            substitute tyVar (TypeVar fresh) monotype

                    -- τ ~ τ1[α/tv]
                    let equalityConstraint = Equality monotype instantiatedMonotype

                    tell (instantiatedConstraint <> equalityConstraint)

                    pure (Constructor (Located loc name), instantiatedMonotype)

        -- VAR
        -- local variables
        Var v'@(Located _ (Local (Located _ v))) -> do
            local <- lookupLocalVar v
            pure (Var v', local)
        -- global variables
        Var (Located l v) -> do
            -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
            varType <- lookupType (TermVarKey $ stripLocation v) env
            case varType of
                Lifted monotype -> pure (Var (Located l v), monotype)
                (Forall tyVar constraint monotype) -> do
                    debugPretty ("Constraint for " <> pretty v <> "= " <> pretty constraint)
                    -- tv
                    fresh <- makeUniqueTyVar -- make a fresh type variable for the type of the variable
                    let
                        -- Q1[α/tv]
                        instantiatedConstraint =
                            substitute tyVar (TypeVar fresh) constraint
                        -- τ1[α/tv]
                        instantiatedMonotype =
                            substitute tyVar (TypeVar fresh) monotype

                    -- τ ~ τ1[α/tv]
                    let equalityConstraint = Equality monotype instantiatedMonotype

                    tell (instantiatedConstraint <> equalityConstraint)

                    pure (Var (Located l v), instantiatedMonotype)

        -- ABS
        Lambda (Located paramLoc (TypedLambdaParam (paramName, expectedParamType))) body -> do
            paramTyVar <- makeUniqueTyVar

            (typedBody, bodyType) <- withLocalType paramName (TypeVar paramTyVar) $ do
                generateConstraints env body

            let functionType = (TypeVar paramTyVar) `Function` bodyType

            pure
                ( Lambda (Located paramLoc (TypedLambdaParam (paramName, (TypeVar paramTyVar)))) typedBody
                , functionType
                )

        -- APP
        FunctionCall e1 e2 -> do
            (e1', t1) <- generateConstraints env e1
            (e2', t2) <- generateConstraints env e2

            resultTyVar <- makeUniqueTyVar

            let equalityConstraint = Equality t1 (Function t2 (TypeVar resultTyVar))
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
            (typedVarExpr, varType) <- withLocalType varName (TypeVar recursiveVar) $ do
                generateConstraints env varExpr

            let recursiveConstraint = Equality (TypeVar recursiveVar) varType
            tell recursiveConstraint

            (typedBody, bodyType) <-
                withLocalType varName (TypeVar recursiveVar) $
                    generateConstraints env body

            pure (LetIn (Located loc varName) NoFieldValue typedVarExpr typedBody, bodyType)

        -- IF
        If cond then' else' -> do
            (typedCond, condType) <- generateConstraints env cond
            (typedThen, thenType) <- generateConstraints env then'
            (typedElse, elseType) <- generateConstraints env else'

            let equalityConstraint = Equality condType (Scalar ScalarBool)
            tell equalityConstraint

            let equalityConstraint1 = Equality thenType elseType
            tell equalityConstraint1

            pure (If typedCond typedThen typedElse, thenType)
        TypeApplication e ty -> do
            error "i dont know what to do with type applications yet sorry"
        Match e cases -> do
            (typedE, eType) <- generateConstraints env e

            resultTyVar <- makeUniqueTyVar

            cases' <- for cases $ \(pattern, body) -> scoped $ do
                tell EmptyConstraint

                (typedPattern, patternType) <- generatePatternConstraints pattern

                let equalityConstraint = Equality eType patternType
                tell equalityConstraint

                (typedBody, bodyType) <- generateConstraints env body

                tell (Equality bodyType (TypeVar resultTyVar))

                pure (typedPattern, typedBody)

            pure (Match typedE cases', TypeVar resultTyVar)
        Block exprs -> do
            vals <- for exprs $ \expr -> do
                generateConstraints env expr

            let exprs = fmap fst vals

            let exprTypes = fmap snd vals

            pure $ ((Syntax.Block exprs), last exprTypes)
        Let (Located loc varName) NoFieldValue varExpr -> do
            recursiveVar <- makeUniqueTyVar
            (typedVarExpr, varType) <- withLocalType varName (TypeVar recursiveVar) $ do
                generateConstraints env varExpr

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
        VarPattern (Located loc varName) -> do
            varType <- makeUniqueTyVar
            modify (addLocalType (NormalVarName <$> varName) (TypeVar varType))

            pure (VarPattern (Located loc (NormalVarName <$> varName)), TypeVar varType)

solveConstraints :: Pretty loc => AxiomScheme loc -> Constraint loc -> Constraint loc -> Sem '[Error UnifyError] (Constraint loc, Substitution loc)
solveConstraints axioms given wanted = do
    (substitution, simplifiedWanted) <- unifyEquality wanted

    -- let simplifiedGiven = substitute substitution given

    -- let simplifiedWanted' = substitute substitution simplifiedWanted

    -- let (entailable, residualWanted) = entail axioms simplifiedGiven simplifiedWanted'

    pure (todo, substitution)

unifyEquality :: Pretty loc => Constraint loc -> Sem '[Error UnifyError] (Substitution loc, Constraint loc)
unifyEquality (Equality a b) = unify a b
unifyEquality (Conjunction a b) = do
    -- a lot of this is duplicated from unifyMany which is a bit annoying
    (s1, c1) <- unifyEquality a
    -- apply s1 to b before unifying
    (s2, c2) <- unifyEquality (substituteAll s1 b)
    pure (s1 <> s2, c1 <> c2)
unifyEquality EmptyConstraint = pure (mempty, EmptyConstraint)

unify ::
    HasCallStack =>
    Monotype loc -> Monotype loc -> Sem '[Error UnifyError] (Substitution loc, Constraint loc)
unify (TypeVar a) b = (,EmptyConstraint) <$> bind a b
unify a (TypeVar b) = (,EmptyConstraint) <$> bind b a
unify (Scalar a) (Scalar b) =
    if a == b
        then pure (mempty, EmptyConstraint)
        else throw ScalarMismatch
unify (TypeConstructor a as) (TypeConstructor b bs)
    | a /= b = throw TypeConstructorMismatch
    | length as /= length bs = throw ArityMismatch
    | otherwise = unifyMany as bs
unify (Function a b) (Function c d) = unifyMany [a, b] [c, d]
unify a b = throw $ UnificationFailed $ "Unification failed: " <> show a <> " and " <> show b

bind :: Member (Error UnifyError) r => UniqueTyVar -> Monotype loc -> Sem r (Substitution loc)
bind a t | t == TypeVar a = pure mempty
bind a t | occurs a t = throw OccursCheckFailed
bind a t = pure $ substitution (a, t)

unifyMany ::
    HasCallStack =>
    [Monotype loc] ->
    [Monotype loc] ->
    Sem '[Error UnifyError] (Substitution loc, Constraint loc)
unifyMany [] [] = pure (mempty, EmptyConstraint)
unifyMany [] _ = throw UnifyMismatch
unifyMany _ [] = throw UnifyMismatch
unifyMany (a : as) (b : bs) = do
    (s1 :: Substitution loc, c1) <- unify a b
    (s2, c2) <- unifyMany (fmap (substituteAll s1) as) (fmap (substituteAll s1) bs)
    pure (s1 <> s2, c1 <> c2)

data UnifyError
    = OccursCheckFailed
    | ScalarMismatch
    | TypeConstructorMismatch
    | ArityMismatch
    | UnificationFailed String
    | UnifyMismatch
    deriving (Eq, Show)
