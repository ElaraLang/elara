{-# LANGUAGE PatternSynonyms #-}

module Infer.Common where

import Control.Exception (throwIO)
import Elara.AST.Generic hiding (Type)
import Elara.AST.StripLocation
import Elara.AST.Typed (TypedExpr)
import Elara.Data.Pretty (AnsiStyle, Doc, Pretty)
import Elara.Data.TopologicalGraph (createGraph)
import Elara.Data.Unique (uniqueGenToIO)
import Elara.Desugar (DesugarError, DesugarState (..), desugarExpr)
import Elara.Error (runDiagnosticWriter, runErrorOrReport)
import Elara.Error.Effect (addFile)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle)
import Elara.Parse.Expression (exprParser)
import Elara.Parse.Primitives (IsParser (..))
import Elara.Parse.Stream
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Rename (RenameError, renameExpr)
import Elara.Shunt (OpTable, ShuntError, shuntExpr)
import Elara.TypeInfer (completeExpression, inferExpression)
import Elara.TypeInfer.Domain (Domain)
import Elara.TypeInfer.Error (TypeInferenceError)
import Elara.TypeInfer.Infer (initialStatus)
import Elara.TypeInfer.Infer qualified as Infer
import Elara.TypeInfer.Type (Type (..))
import Elara.TypeInfer.Type qualified as Type
import Error.Diagnose (Diagnostic, TabSize (..), WithUnicode (WithUnicode), defaultStyle, printDiagnostic)
import Parse.Common (lexAndParse)
import Polysemy
import Polysemy.Error (Error, errorToIOFinal)
import Polysemy.Maybe (runMaybe)
import Polysemy.Reader (runReader)
import Polysemy.State (State, evalState)
import Polysemy.Writer (runWriter)
import Print (showPretty)
import Test.HUnit (assertFailure)
import Text.Megaparsec (eof)

pattern Forall' :: Text -> Domain -> Type () -> Type ()
pattern Forall' name domain t = Forall () () name domain t

pattern Function' :: Type () -> Type () -> Type ()
pattern Function' a b = Function () a b

pattern VariableType' :: Text -> Type ()
pattern VariableType' name = VariableType () name

pattern Tuple' :: NonEmpty (Type ()) -> Type ()
pattern Tuple' ts = Type.Tuple () ts

completeInference :: Member (State Infer.Status) r => TypedExpr -> Sem r TypedExpr
completeInference x = do
    ctx <- Infer.getAll
    completeExpression ctx x

inferPipeline :: _ => Text -> Sem r TypedExpr
inferPipeline =
    lexAndParse (exprParser <* fromParsec eof)
        >=> (subsume_ . desugarExpr)
        >=> renameExpr
        >=> shuntExpr
        >=> flip inferExpression Nothing
        >=> completeInference

errorToIOFinal' :: forall e r a. (Member (Final IO) r, Exception e) => Sem (Error e ': r) a -> Sem r a
errorToIOFinal' sem = do
    res <- errorToIOFinal sem
    case res of
        Left e -> embedFinal $ throwIO e
        Right a -> pure a

runInferPipeline :: Text -> IO (Diagnostic _, Maybe TypedExpr)
runInferPipeline t =
    runFinal
        . runDiagnosticWriter
        . (\x -> addFile "" (toString t) *> runMaybe x)
        . fmap snd
        . runWriter
        . embedToFinal
        . uniqueGenToIO
        . evalState primitiveRenameState
        . runErrorOrReport @DesugarError
        . runErrorOrReport @(WParseErrorBundle TokenStream ElaraParseError)
        . runReader (createGraph []) -- Module graph
        . evalState (DesugarState mempty)
        . runErrorOrReport @RenameError
        . runReader @OpTable (fromList []) -- Operator table
        . runErrorOrReport @ShuntError
        . evalState initialStatus
        . runErrorOrReport @TypeInferenceError
        . inferPipeline
        $ t

diagShouldSucceed :: (Diagnostic (Doc AnsiStyle), Maybe b) -> IO b
diagShouldSucceed (d, x) = do
    printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle d
    case x of
        Just x -> pure x
        Nothing -> error "Expected successful inference"

typeOf' :: MonadIO m => Text -> m (Type ())
typeOf' msg = do
    x <- liftIO $ runInferPipeline msg
    y <- liftIO $ diagShouldSucceed x
    pure $ stripLocation $ typeOf y

failTypeMismatch :: Pretty a1 => String -> String -> a1 -> IO a2
failTypeMismatch name expected actual =
    assertFailure
        ("Expected " <> name <> " to have type " <> expected <> " but was " <> toString (showPretty actual))

inferSpec :: (MonadIO m, Pretty a1) => Text -> String -> m (Type (), a1 -> IO a2)
inferSpec code expected = do
    t' <- typeOf' code
    pure (t', failTypeMismatch (toString code) expected)
