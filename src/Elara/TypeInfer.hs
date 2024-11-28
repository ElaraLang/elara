{-# LANGUAGE BlockArguments #-}

module Elara.TypeInfer where

import Data.Containers.ListUtils (nubOrd)
import Data.Generics.Product
import Data.Generics.Wrapped
import Data.List ((\\))
import Data.Map qualified as Map
import Elara.AST.Generic (
    Declaration (Declaration),
    Declaration' (Declaration'),
    DeclarationBody (DeclarationBody),
 )
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Generic.Types (
    Declaration (Declaration),
    Declaration' (Declaration'),
    DeclarationBody (DeclarationBody),
    DeclarationBody' (..),
 )
import Elara.AST.Kinded
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName, Name (..), NameLike (nameText), Qualified (..), VarName)
import Elara.AST.Region (IgnoreLocation (..), Located (Located), SourceRegion, unlocated)
import Elara.AST.Select (
    LocatedAST (
        Shunted,
        Typed
    ),
 )
import Elara.TypeInfer.Type (AxiomScheme (EmptyAxiomScheme), Constraint (..), Monotype (TypeVar), Substitutable (substituteAll), Type (..), TypeVariable (..))

import Data.Set (difference)
import Elara.AST.Shunted as Shunted
import Elara.AST.StripLocation (StripLocation (..))
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (VarRef' (..), mkGlobal')
import Elara.Data.Kind (ElaraKind (..))
import Elara.Data.Kind.Infer (InferState, inferKind, inferTypeKind, initialInferState)
import Elara.Data.Unique (Unique, UniqueGen, uniqueGenToIO)
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug, debug, debugWith, structuredDebugToLog)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.Prim (fullListName, primRegion)
import Elara.TypeInfer.ConstraintGeneration (Infer, InferEffects, UnifyError (..), generateConstraints, runInferEffects, solveConstraints)
import Elara.TypeInfer.Environment (InferError, LocalTypeEnvironment, TypeEnvKey (..), TypeEnvironment, addType, addType', withLocalType)
import Elara.TypeInfer.Ftv
import Elara.TypeInfer.Unique (makeUniqueTyVar)
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.State
import Polysemy.Writer (listen)
import Print
import Relude.Extra.Type (type (++))
import Elara.Data.Pretty

type InferPipelineEffects = '[StructuredDebug, State InferState, UniqueGen, Error (UnifyError SourceRegion)] ++ (InferEffects SourceRegion)

runInferPipeline :: forall r a. IsPipeline r => Sem (EffectsAsPrefixOf InferPipelineEffects r) a -> Sem r a
runInferPipeline e = do
    let e' =
            e
                & subsume_
                & structuredDebugToLog
                & evalState initialInferState
                & uniqueGenToIO
                & runErrorOrReport @(UnifyError SourceRegion)

    snd <$> runInferEffects e'

inferModule ::
    forall r.
    (Members InferPipelineEffects r, Infer SourceRegion r) =>
    Module 'Shunted ->
    Sem r (Module 'Typed)
inferModule m = do
    m' <- traverseModuleRevTopologically inferDeclaration m
    pure (m')

inferDeclaration ::
    forall r.
    (HasCallStack, Members InferPipelineEffects r, Infer SourceRegion r) =>
    ShuntedDeclaration ->
    Sem r TypedDeclaration
inferDeclaration (Declaration ld) = do
    Declaration
        <$> traverseOf
            unlocated
            ( \d' -> do
                let (DeclarationBody ldb) = d' ^. field' @"body"
                db' <-
                    DeclarationBody
                        <$> traverseOf
                            unlocated
                            (inferDeclarationBody')
                            ldb
                pure (Declaration' (d' ^. field' @"moduleName") db')
            )
            ld
  where
    inferDeclarationBody' ::
        HasCallStack =>
        ShuntedDeclarationBody' ->
        Sem r TypedDeclarationBody'
    inferDeclarationBody' declBody = case declBody of
        Value name e NoFieldValue valueType annotations -> do
            (typedExpr, polytype) <- inferValue name e
            debugPretty polytype
            pure (Value name typedExpr NoFieldValue NoFieldValue (Generic.coerceValueDeclAnnotations annotations))

inferValue ::
    forall r.
    ( HasCallStack
    , Member UniqueGen r
    , Infer SourceRegion r
    , Member (Error (UnifyError SourceRegion)) r
    ) =>
    Located (Qualified VarName) ->
    ShuntedExpr ->
    Sem r (TypedExpr, Type SourceRegion)
inferValue valueName valueExpr = do
    -- generate
    a <- UnificationVar <$> makeUniqueTyVar
    addType' (TermVarKey (valueName ^. unlocated)) (Lifted $ TypeVar a)
    (constraint, (typedExpr, t)) <- listen $ generateConstraints valueExpr
    debug $ "Generated constraints: " <> pretty constraint <> " for " <> pretty valueName
    let eq = Equality (TypeVar a) t
    (finalConstraint, subst) <- solveConstraints EmptyAxiomScheme EmptyConstraint (eq <> constraint)
    when (finalConstraint /= EmptyConstraint) do
        throw (UnresolvedConstraint finalConstraint)

    let newType = substituteAll subst t

    generalized <- generalize newType

    pure (typedExpr, generalized)

generalize :: forall r. Infer SourceRegion r => Monotype SourceRegion -> Sem r (Type SourceRegion)
generalize ty = do
    env <- get
    let freeVars = ftv ty
    let envVars = freeVars `difference` ftv env
    envVarsAsUniVars <- for (toList envVars) $ \case 
            UnificationVar a -> pure a
            SkolemVar a -> error "Skolem vars should not be in the environment"
    let generalized = Forall (toList envVarsAsUniVars) EmptyConstraint ty
    pure generalized