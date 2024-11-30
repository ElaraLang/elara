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
import Elara.TypeInfer.Type (AxiomScheme (EmptyAxiomScheme), Constraint (..), Monotype (TypeVar), Polytype (..), Substitutable (..), Type (..), TypeVariable (..))

import Elara.AST.Shunted as Shunted
import Elara.AST.Typed as Typed
import Elara.Data.Kind.Infer (InferState, initialInferState)
import Elara.Data.Pretty
import Elara.Data.Unique (UniqueGen, uniqueGenToIO)
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug, debug)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Convert (TypeConvertError, astTypeToGeneralisedInferType, astTypeToInferType)
import Elara.TypeInfer.Environment (TypeEnvKey (..), addType')
import Elara.TypeInfer.Ftv (Fuv (..))
import Elara.TypeInfer.Generalise
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Unique (makeUniqueTyVar)
import Polysemy hiding (transform)
import Polysemy.Error (Error, throw)
import Polysemy.State
import Polysemy.Writer (listen)
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
            expectedType <- traverse astTypeToGeneralisedInferType valueType
            debug $ "Expected type for " <> pretty name <> ": " <> pretty expectedType
            (typedExpr, polytype) <- inferValue (name ^. unlocated) e expectedType
            debug $ "Inferred type for " <> pretty name <> ": " <> pretty polytype
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
    expected <- case expectedType of
        Just t -> pure t
        Nothing -> Lifted . TypeVar . UnificationVar <$> makeUniqueTyVar
    expectedAsMono <- instantiate expected
    addType' (TermVarKey (valueName)) expected
    (constraint, (typedExpr, t)) <- listen $ generateConstraints valueExpr

    let constraint' = constraint <> Equality expectedAsMono t
    let tch = fuv t <> fuv constraint'
    debug $ "Generated constraints: " <> pretty constraint' <> " for " <> pretty valueName
    debug $ "Type: " <> pretty t
    debug $ "tch: " <> pretty tch

    (finalConstraint, subst) <- solveConstraint mempty tch (constraint')

    when (finalConstraint /= EmptyConstraint) do
        throw (UnresolvedConstraint valueName finalConstraint)

    let newType = substituteAll subst t

    debug $ "Substituted type: " <> pretty newType <> " from " <> pretty t <> " with " <> pretty subst

    generalized <- generalise (removeSkolems newType)

    pure (getExpr (substituteAll subst (SubstitutableExpr typedExpr)), generalized)

newtype SubstitutableExpr loc = SubstitutableExpr {getExpr :: TypedExpr} deriving (Show, Eq, Ord)

instance Substitutable SubstitutableExpr SourceRegion where
    substitute tv t (SubstitutableExpr (Generic.Expr (e, exprType))) = do
        let exprType' = substitute tv t exprType
        let e' =
                -- recursively apply subst to the children
                over
                    (gplate @(Monotype SourceRegion) @TypedExpr')
                    (\exp -> (substitute tv t exp))
                    (e ^. unlocated)

        SubstitutableExpr (Generic.Expr (e' <$ e, exprType'))
