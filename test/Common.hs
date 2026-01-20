-- | Common test utilities and assertions
module Common (
    -- * Assertions
    (<=>),
    diagShouldSucceed,
    diagShouldFail,
    evalReportableM,

    -- * AST Utilities
    stripInParens,

    -- * Effect Runners
    runUnique,
) where

import Control.Exception.Safe (MonadCatch)
import Effectful (Eff, IOE, runEff, runPureEff)
import Elara.AST.Generic (Expr (..), Expr' (..))
import Elara.Data.Pretty (AnsiStyle, Doc, prettyToText)
import Elara.Data.Unique.Effect (UniqueGen, uniqueGenToGlobalIO)
import Elara.Error (ReportableError, report, runDiagnosticWriter)
import Error.Diagnose (Diagnostic, TabSize (..), WithUnicode (..), hasReports, prettyDiagnostic')
import Hedgehog.Internal.Property (MonadTest, failWith)
import Orphans ()
import Test.HUnit (assertFailure)
import Test.Syd (Expectation, shouldBe)

(<=>) :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
(<=>) = shouldBe

-- | Assert that a diagnostic indicates success, returning the successful value.
diagShouldSucceed :: MonadTest m => (Diagnostic (Doc AnsiStyle), Maybe b) -> m b
diagShouldSucceed (d, x) = withFrozenCallStack $ do
    when (hasReports d) $ failWith Nothing $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d
    case x of
        Just ok -> pure ok
        Nothing -> failWith Nothing $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d

-- | Assert that a diagnostic indicates failure.
diagShouldFail :: MonadIO m => (Diagnostic (Doc AnsiStyle), Maybe b) -> m ()
diagShouldFail (d, x) = liftIO $ do
    unless (hasReports d) $ assertFailure "Expected diagnostic to fail, but succeeded."
    case x of
        Just _ -> assertFailure "Expected diagnostic to fail, but succeeded."
        Nothing -> pass

-- | Evaluate a computation that may return a reportable error, failing the test with a pretty-printed
evalReportableM ::
    ReportableError e =>
    (MonadTest m, Show x, MonadCatch m, HasCallStack) =>
    m (Either e x) -> m x
evalReportableM m = do
    r <- m
    case r of
        Left err -> do
            let (x, _) = runPureEff $ runDiagnosticWriter $ report err
            failWith Nothing $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) x
        Right ok -> pure ok

{- | Remove any 'InParens' wrappers from an expression
Useful for comparing the overall structure of expressions without worrying about parentheses
-}
stripInParens :: _ => Expr ast -> Expr ast
stripInParens = transform $ \case
    (Expr (InParens x, _)) -> x
    x -> x

-- | Run an effectful computation that requires unique generation
runUnique :: MonadIO m => Eff [UniqueGen, IOE] a -> m a
runUnique = liftIO . runEff . uniqueGenToGlobalIO
