{-# LANGUAGE PatternSynonyms #-}

module Infer.Common where

import Common (diagShouldFail, diagShouldSucceed)
import Control.Exception (throwIO)
import Elara.AST.Generic hiding (Type)
import Elara.AST.Module (Module)
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.AST.StripLocation
import Elara.AST.Typed (TypedExpr)
import Elara.Data.Pretty (Pretty)
import Elara.Data.TopologicalGraph (createGraph)
import Elara.Data.Unique
import Elara.Desugar (desugar, desugarExpr, runDesugar, runDesugarPipeline)
import Elara.Error (addFile)
import Elara.Lexer.Pipeline (runLexPipeline)
import Elara.Lexer.Reader (readTokensWith)
import Elara.Logging (StructuredDebug)
import Elara.Parse (moduleParser, parsePipeline, runParsePipeline)
import Elara.Parse.Expression (exprParser)
import Elara.Pipeline (PipelineRes, finalisePipeline)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Rename (rename, renameExpr, runRenamePipeline)
import Elara.Shunt (runShuntPipeline, shunt, shuntExpr)
import Elara.TypeInfer (completeExpression, inferExpression, inferModule, runInferPipeline)
import Elara.TypeInfer.Domain (Domain)
import Elara.TypeInfer.Error
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Type (Type (..))
import Elara.TypeInfer.Type qualified as Type
import Elara.TypeInfer.Unique
import Hedgehog
import Hedgehog.Internal.Property (failWith)
import Polysemy
import Polysemy.Error (Error, errorToIOFinal)
import Polysemy.Log
import Polysemy.Reader (runReader)
import Polysemy.State (State)
import Print (showPretty)

pattern Forall' :: UniqueTyVar -> Domain -> Type () -> Type ()
pattern Forall' name domain t = Forall () () name domain t

pattern Function' :: Type () -> Type () -> Type ()
pattern Function' a b = Function () a b

pattern VariableType' :: UniqueTyVar -> Type ()
pattern VariableType' name = VariableType () name

completeInference :: (Member (State Infer.Status) r, Member UniqueGen r, Member (Error TypeInferenceError) r, Member StructuredDebug r) => TypedExpr -> Sem r TypedExpr
completeInference x = do
    ctx <- Infer.getAll
    completeExpression ctx x

inferFully :: ToString a => a -> PipelineRes TypedExpr
inferFully source = finalisePipeline . runInferPipeline . runShuntPipeline . runParsePipeline . runLexPipeline $ do
    let fp = "<tests>"
    addFile fp (toString source)
    tokens <- readTokensWith fp (toString source)
    parsed <- parsePipeline exprParser fp (toString source, tokens)
    desugared <- runDesugarPipeline $ runDesugar $ desugarExpr parsed
    renamed <-
        runRenamePipeline
            (createGraph [])
            primitiveRenameState
            ( runReader (Nothing @(Module 'Desugared)) $
                runReader (Nothing @(Declaration 'Desugared)) $
                    renameExpr desugared
            )
    shunted <- runReader mempty $ shuntExpr renamed
    inferExpression shunted Nothing >>= completeInference

inferModuleFully :: ToString a => a -> PipelineRes (Module Typed, Map (Qualified Name) (Type SourceRegion))
inferModuleFully source = finalisePipeline . runInferPipeline . runShuntPipeline . runParsePipeline . runLexPipeline $ do
    let fp = "<tests>"
    addFile fp (toString source)
    tokens <- readTokensWith fp (toString source)
    parsed <- parsePipeline moduleParser fp (toString source, tokens)
    desugared <- runDesugarPipeline $ runDesugar $ desugar parsed
    renamed <- runRenamePipeline (createGraph []) primitiveRenameState (runReader Nothing $ rename desugared)
    shunted <- runReader mempty $ shunt renamed
    inferModule shunted

errorToIOFinal' :: forall e r a. (Member (Final IO) r, Exception e) => Sem (Error e ': r) a -> Sem r a
errorToIOFinal' sem = do
    res <- errorToIOFinal sem
    case res of
        Left e -> embedFinal $ throwIO e
        Right a -> pure a

typeOf' :: (MonadIO m, MonadTest m) => Text -> m (Type ())
typeOf' msg = do
    x <- liftIO $ inferFully msg
    y <- diagShouldSucceed x
    pure $ stripLocation $ typeOf y

failTypeMismatch :: (Pretty a1, MonadTest m) => String -> String -> a1 -> m a2
failTypeMismatch name expected actual =
    withFrozenCallStack $
        failWith
            Nothing
            ("Expected " <> name <> " to have type " <> expected <> " but was " <> toString (showPretty actual))

inferSpec :: (MonadIO m, Pretty a1, MonadTest m) => Text -> String -> m (Type (), a1 -> m a2)
inferSpec code expected = do
    t' <- typeOf' code
    pure (t', failTypeMismatch (toString code) expected)

inferShouldFail :: MonadIO m => Text -> m ()
inferShouldFail code = do
    x <- liftIO $ inferFully code
    diagShouldFail x
