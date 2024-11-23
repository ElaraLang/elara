module Elara.TypeInfer.ConstraintGeneration where

import Data.Map qualified as Map
import Elara.AST.Generic (Expr (..), Expr' (..))
import Elara.AST.Generic.Common (NoFieldValue (NoFieldValue))
import Elara.AST.Generic.Types (TypedLambdaParam (..))
import Elara.AST.Region (Located (Located), SourceRegion)
import Elara.AST.Shunted (ShuntedExpr, ShuntedExpr')
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.AST.Typed (TypedExpr, TypedExpr')
import Elara.AST.VarRef
import Elara.Data.Unique (UniqueGen)
import Elara.TypeInfer.Environment (InferError, LocalTypeEnvironment, TypeEnvKey (TermVarKey), TypeEnvironment, addType, lookupType, withLocalType)
import Elara.TypeInfer.Type (AxiomScheme, Constraint (..), Monotype (..), Scalar (..), Substitutable (..), Substitution, Type (Forall))
import Elara.TypeInfer.Unique (makeUniqueTyVar)
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Writer
import Print (debugColored, debugPretty)
import TODO (todo)

type InferEffects loc = '[Writer (Constraint loc), State (LocalTypeEnvironment loc), Error (InferError loc), UniqueGen]
type Infer loc r = Members (InferEffects loc) r

generateConstraints :: Infer SourceRegion r => TypeEnvironment SourceRegion -> ShuntedExpr -> Sem r (TypedExpr, Monotype SourceRegion)
generateConstraints env (Expr (Located loc expr', expectedType)) = do
    (typedExpr', monotype) <- generateConstraints' env expr'
    pure (Expr (Located loc typedExpr', monotype), monotype)

liftMonotype :: Infer loc r => Monotype loc -> Sem r (Type loc)
liftMonotype m = do
    fresh <- makeUniqueTyVar
    -- TODO this is dumb
    pure $ Forall fresh EmptyConstraint m

generateConstraints' :: Infer SourceRegion r => TypeEnvironment SourceRegion -> ShuntedExpr' -> Sem r (TypedExpr', Monotype SourceRegion)
generateConstraints' env expr' =
    case expr' of
        Int i -> pure (Int i, Scalar ScalarInt)
        Float f -> pure (Float f, Scalar ScalarFloat)
        String s -> pure (String s, Scalar ScalarString)
        Char c -> pure (Char c, Scalar ScalarChar)
        Unit -> pure (Unit, Scalar ScalarUnit)
        -- VAR
        Var (Located l v) -> do
            -- (ν:∀a.Q1 ⇒ τ1) ∈ Γ
            polyType@(Forall tyVar constraint monotype) <- lookupType (TermVarKey $ stripLocation v) env

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

            let functionType = Function (TypeVar paramTyVar) bodyType

            pure (Lambda (Located paramLoc (TypedLambdaParam (paramName, (TypeVar paramTyVar)))) typedBody, functionType)

        -- APP
        FunctionCall e1 e2 -> do
            (c1, (e1', t1)) <- listen $ generateConstraints env e1
            (c2, (e2', t2)) <- listen $ generateConstraints env e2

            resultTyVar <- makeUniqueTyVar

            let equalityConstraint = Equality t1 (Function t2 (TypeVar resultTyVar))
            let conjunct = mconcat [c1, c2, equalityConstraint]

            tell conjunct

            pure (FunctionCall e1' e2', TypeVar resultTyVar)

tidyConstraint :: Constraint loc -> Constraint loc
tidyConstraint EmptyConstraint = EmptyConstraint
tidyConstraint (Conjunction x y) | x == y = tidyConstraint x
tidyConstraint (Conjunction x (Conjunction y z)) | x == y = Conjunction (tidyConstraint x) (tidyConstraint z)
tidyConstraint (Conjunction c1 c2) = Conjunction (tidyConstraint c1) (tidyConstraint c2)
tidyConstraint (Equality x y) = Equality x y

-- unifyConstraints :: [Constraint loc] ->

-- simplifyConstraints :: AxiomScheme loc -> Constraint loc -> Constraint loc -> (Constraint loc, Substitution loc)
-- simplifyConstraints axioms given wanted = (residual, subst)
--   where
--     (residual, subst) = todo
