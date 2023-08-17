module Infer where

import Control.Exception (throwIO)
import Elara.AST.Typed (TypedExpr)
import Elara.Data.TopologicalGraph (createGraph)
import Elara.Data.Unique (uniqueGenToIO)
import Elara.Desugar (DesugarError, DesugarState (..), desugarExpr)
import Elara.Error (runDiagnosticWriter, runErrorOrReport)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle)
import Elara.Parse.Expression (exprParser)
import Elara.Parse.Stream
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Rename (RenameError, renameExpr)
import Elara.Shunt (OpTable, ShuntError, shuntExpr)
import Elara.TypeInfer (completeExpression, inferExpression)
import Elara.TypeInfer.Error (TypeInferenceError)
import Elara.TypeInfer.Infer (initialStatus)
import Elara.TypeInfer.Infer qualified as Infer
import Error.Diagnose (Diagnostic, TabSize (..), WithUnicode (WithUnicode), defaultStyle, printDiagnostic)
import Parse.Common (lexAndParse)
import Polysemy
import Polysemy.Error (Error, errorToIOFinal)
import Polysemy.Maybe (runMaybe)
import Polysemy.Reader (runReader)
import Polysemy.State (evalState)
import Polysemy.Writer (runWriter)
import Print (printPretty)
import Test.Hspec

completeInference x = do
    ctx <- Infer.getAll
    completeExpression ctx x

inferPipeline :: _ => Text -> Sem r TypedExpr
inferPipeline =
    lexAndParse exprParser
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
runInferPipeline =
    runFinal
        . runDiagnosticWriter
        . runMaybe
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

spec :: Spec
spec = do
    (d, x) <- runIO $ runInferPipeline "\\x -> "
    runIO $ printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle d
    runIO $ printPretty x