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
import Elara.TypeInfer.Type (Monotype (TypeVar), Type (..))

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
import Elara.TypeInfer.ConstraintGeneration (generateConstraints)
import Elara.TypeInfer.Environment (TypeEnvKey (..), addType, addType', withLocalType)
import Elara.TypeInfer.Unique (makeUniqueTyVar)
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.State
import Print
import TODO

type InferPipelineEffects = '[StructuredDebug, State InferState, UniqueGen]

runInferPipeline :: forall r a. IsPipeline r => Sem (EffectsAsPrefixOf InferPipelineEffects r) a -> Sem r a
runInferPipeline e = do
    e
        & subsume_
        & structuredDebugToLog
        & evalState initialInferState
        & uniqueGenToIO

inferModule ::
    forall r.
    Members InferPipelineEffects r =>
    Module 'Shunted ->
    Sem r (Module 'Typed, Map (Qualified Name) (Type SourceRegion))
inferModule m = todo

inferDeclaration ::
    forall r.
    (HasCallStack, Member UniqueGen r, _) =>
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
            e' <- inferValue name e
            pure (Value name e' NoFieldValue NoFieldValue (Generic.coerceValueDeclAnnotations annotations))

inferValue ::
    forall r.
    (HasCallStack, Member UniqueGen r) =>
    Located (Qualified VarName) ->
    ShuntedExpr ->
    Sem r TypedExpr
inferValue valueName valueExpr = do
    -- generate
    a <- makeUniqueTyVar
    -- addType' (TermVarKey (valueName ^. unlocated)) (Lifted $ TypeVar a)
    -- t <- withLocalType (valueName ^. unlocated) (TypeVar a) $ do
    --     generateConstraints valueExpr

    pure _
