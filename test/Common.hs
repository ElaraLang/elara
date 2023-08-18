module Common where

import Elara.Data.Pretty
import Error.Diagnose (Diagnostic, TabSize (..), WithUnicode (..), hasReports, prettyDiagnostic')
import Test.HUnit (assertFailure)
import Test.Hspec

(<=>) :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
(<=>) = shouldBe

diagShouldSucceed :: MonadIO m => (Diagnostic (Doc AnsiStyle), Maybe b) -> m b
diagShouldSucceed (d, x) = liftIO $ do
    when (hasReports d) $ assertFailure $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d
    case x of
        Just ok -> pure ok
        Nothing -> assertFailure $ toString $ prettyToText $ prettyDiagnostic' WithUnicode (TabSize 4) d
