{-# LANGUAGE BlockArguments #-}

module Elara.TypeInfer where

import Data.Containers.ListUtils (nubOrd)
import Data.Generics.Product
import Data.Generics.Wrapped
import Data.List ((\\))
import Data.Map qualified as Map
import Elara.AST.Generic ()
import Elara.AST.Generic hiding (Type)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Generic.Common
import Elara.AST.Kinded
import Elara.AST.Module
import Elara.TypeInfer.Type (Type (..))
import Elara.AST.Name (LowerAlphaName, Name (..), NameLike (nameText), Qualified (..))
import Elara.AST.Region (IgnoreLocation (..), Located (Located), SourceRegion, unlocated)
import Elara.AST.Select (
    LocatedAST (
        Shunted,
        Typed
    ),
 )

import TODO
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
import Polysemy hiding (transform)
import Polysemy.Error (Error, mapError, throw)
import Polysemy.State
import Print

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
                            (inferDeclarationBody' (d' ^. field' @"name"))
                            ldb
                pure (Declaration' (d' ^. field' @"moduleName") (d' ^. field' @"name") db')
            )
            ld
  where
    inferDeclarationBody' ::
        HasCallStack =>
        Located (Qualified Name) ->
        ShuntedDeclarationBody' ->
        Sem r TypedDeclarationBody'
    inferDeclarationBody' = todo


