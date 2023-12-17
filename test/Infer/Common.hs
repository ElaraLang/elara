{-# LANGUAGE PatternSynonyms #-}

module Infer.Common where

import Common (diagShouldFail, diagShouldSucceed)
import Control.Exception (throwIO)
import Elara.AST.Generic hiding (Type)
import Elara.AST.StripLocation
import Elara.AST.Typed (TypedExpr)
import Elara.Data.Pretty (Pretty)
import Elara.Data.TopologicalGraph (createGraph)
import Elara.Data.Unique
import Elara.Desugar (desugarExpr, runDesugar, runDesugarPipeline)
import Elara.Lexer.Pipeline (runLexPipeline)
import Elara.Lexer.Reader (readTokensWith)
import Elara.Parse (parsePipeline, runParsePipeline)
import Elara.Parse.Expression (exprParser)
import Elara.Pipeline (PipelineRes, finalisePipeline)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Rename (renameExpr, runRenamePipeline)
import Elara.Shunt (runShuntPipeline, shuntExpr)
import Elara.TypeInfer (completeExpression, inferExpression, runInferPipeline)
import Elara.TypeInfer.Domain (Domain)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Type (Type (..))
import Elara.TypeInfer.Type qualified as Type
import Elara.TypeInfer.Unique
import Hedgehog
import Hedgehog.Internal.Property
import Polysemy
import Polysemy.Error (Error, errorToIOFinal)
import Polysemy.Reader (runReader)
import Polysemy.State (State)
import Print (showPretty)
import Test.HUnit (assertFailure)

pattern Forall' :: UniqueTyVar -> Domain -> Type () -> Type ()
pattern Forall' name domain t = Forall () () name domain t

pattern Function' :: Type () -> Type () -> Type ()
pattern Function' a b = Function () a b

pattern VariableType' :: UniqueTyVar -> Type ()
pattern VariableType' name = VariableType () name

pattern Tuple' :: NonEmpty (Type ()) -> Type ()
pattern Tuple' ts = Type.Tuple () ts

completeInference :: (Member (State Infer.Status) r, Member UniqueGen r) => TypedExpr -> Sem r TypedExpr
completeInference x = do
    ctx <- Infer.getAll
    completeExpression ctx x

inferFully :: ToString a => a -> PipelineRes TypedExpr
inferFully source = finalisePipeline . runInferPipeline . runShuntPipeline . runParsePipeline . runLexPipeline $ do
    let fp = "<tests>"
    tokens <- readTokensWith fp (toString source)
    parsed <- parsePipeline exprParser fp tokens
    desugared <- runDesugarPipeline $ runDesugar $ desugarExpr parsed
    renamed <- runRenamePipeline (createGraph []) primitiveRenameState (runReader Nothing $ renameExpr desugared)
    shunted <- runReader mempty $ shuntExpr renamed
    inferExpression shunted Nothing >>= completeInference

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
