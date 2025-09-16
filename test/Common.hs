module Common where

import Control.Exception.Safe
import Effectful
import Elara.AST.Generic
import Elara.Data.Pretty
import Elara.Data.Unique.Effect (UniqueGen, uniqueGenToGlobalIO)
import Elara.Error (ReportableError, report, runDiagnosticWriter)
import Error.Diagnose (Diagnostic, TabSize (..), WithUnicode (..), hasReports, prettyDiagnostic')
import Hedgehog.Internal.Property
import Orphans ()
import Test.HUnit (assertFailure)
import Test.Syd

(<=>) :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
(<=>) = shouldBe

stripInParens :: _ => Expr ast -> Expr ast
stripInParens = transform $ \case
    (Expr (InParens x, _)) -> x
    x -> x

diagShouldSucceed :: MonadTest m => (Diagnostic (Doc AnsiStyle), Maybe b) -> m b
diagShouldSucceed (d, x) = withFrozenCallStack $ do
    when (hasReports d) $ failWith Nothing $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d
    case x of
        Just ok -> pure ok
        Nothing -> failWith Nothing $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d

diagShouldFail :: MonadIO m => (Diagnostic (Doc AnsiStyle), Maybe b) -> m ()
diagShouldFail (d, x) = liftIO $ do
    unless (hasReports d) $ assertFailure "Expected diagnostic to fail, but succeeded."
    case x of
        Just _ -> assertFailure "Expected diagnostic to fail, but succeeded."
        Nothing -> pass

runUnique :: MonadIO m => Eff [UniqueGen, IOE] a -> m a
runUnique = liftIO . runEff . uniqueGenToGlobalIO

evalReportableM :: ReportableError e => (MonadTest m, Show x, MonadCatch m, HasCallStack) => m (Either e x) -> m x
evalReportableM m = do
    r <- m
    case r of
        Left err -> do
            let (x, _) = runPureEff $ runDiagnosticWriter $ report err
            failWith Nothing $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) x
        Right ok -> pure ok
