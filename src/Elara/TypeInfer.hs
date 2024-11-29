{-# LANGUAGE BlockArguments #-}

module Elara.TypeInfer where

import Data.Generics.Product
import Elara.AST.Generic (
    Declaration (Declaration),
    Declaration' (Declaration'),
    DeclarationBody (DeclarationBody),
 )
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Generic.Types (
    DeclarationBody' (..),
 )
import Elara.AST.Module
import Elara.AST.Name (Qualified (..), VarName)
import Elara.AST.Region (SourceRegion, unlocated)
import Elara.AST.Select (
    LocatedAST (
        Shunted,
        Typed
    ),
 )
import Elara.TypeInfer.Type (AxiomScheme (EmptyAxiomScheme), Constraint (..), Monotype (TypeVar), Polytype, Substitutable (substituteAll), Type (..), TypeVariable (..))

import Elara.AST.Shunted as Shunted
import Elara.AST.Typed as Typed
import Elara.Data.Kind.Infer (InferState, initialInferState)
import Elara.Data.Pretty
import Elara.Data.Unique (UniqueGen, uniqueGenToIO)
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug, debug)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.TypeInfer.ConstraintGeneration (UnifyError (..), generateConstraints, runInferEffects, solveConstraints)
import Elara.TypeInfer.Convert (astTypeToInferType, TypeConvertError)
import Elara.TypeInfer.Environment (TypeEnvKey (..), addType')
import Elara.TypeInfer.Generalise
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Unique (makeUniqueTyVar)
import Polysemy hiding (transform)
import Polysemy.Error (Error, throw)
import Polysemy.State
import Polysemy.Writer (listen)
import Print
import Relude.Extra.Type (type (++))

type InferPipelineEffects =
    '[ StructuredDebug
     , State InferState
     , UniqueGen
     , Error (UnifyError SourceRegion)
     , Error (TypeConvertError)
     ]
        ++ (InferEffects SourceRegion)

runInferPipeline :: forall r a. IsPipeline r => Sem (EffectsAsPrefixOf InferPipelineEffects r) a -> Sem r a
runInferPipeline e = do
    let e' =
            e
                & subsume_
                & evalState initialInferState
                & uniqueGenToIO
                & runErrorOrReport @(UnifyError SourceRegion)
                & runErrorOrReport @(TypeConvertError)

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
            expectedType <- traverse astTypeToInferType valueType
            (typedExpr, polytype) <- inferValue (name ^. unlocated) e expectedType
            debugPretty (name, polytype)
            addType' (TermVarKey (name ^. unlocated)) (Polytype polytype)
            pure (Value name typedExpr NoFieldValue NoFieldValue (Generic.coerceValueDeclAnnotations annotations))

inferValue ::
    forall r.
    ( HasCallStack
    , Member UniqueGen r
    , Infer SourceRegion r
    , Member (Error (UnifyError SourceRegion)) r
    ) =>
    (Qualified VarName) ->
    ShuntedExpr ->
    Maybe (Type SourceRegion) ->
    Sem r (TypedExpr, Polytype SourceRegion)
inferValue valueName valueExpr expectedType = do
    -- generate
    a <- UnificationVar <$> makeUniqueTyVar
    addType' (TermVarKey (valueName)) (Lifted $ TypeVar a)
    (constraint, (typedExpr, t)) <- listen $ generateConstraints valueExpr
    debug $ "Generated constraints: " <> pretty constraint <> " for " <> pretty valueName
    debug $ "Type: " <> pretty t
    let eq = Equality (TypeVar a) t

    let expectedTypeConstraint = case expectedType of
            Just (Lifted t') -> Equality t' t
            _ -> EmptyConstraint


    (finalConstraint, subst) <- solveConstraints EmptyAxiomScheme EmptyConstraint (eq <> expectedTypeConstraint <> constraint)

    when (finalConstraint /= EmptyConstraint) do
        throw (UnresolvedConstraint finalConstraint)

    let newType = substituteAll subst t

    generalized <- generalise newType

    pure (typedExpr, generalized)
