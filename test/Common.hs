-- | Common test utilities and assertions
module Common (
    -- * Assertions
    (<=>),
    evalReportableM,

    -- * Effect Runners
)
where

import Hedgehog.Internal.Property (MonadTest, failWith)
import Test.Syd (Expectation, shouldBe)

import Elara.Data.Pretty (prettyToText)
import Elara.Error
import Orphans ()

(<=>) :: (HasCallStack, Eq a, Show a) => a -> a -> Expectation
(<=>) = shouldBe

-- | Evaluate a computation that may return a reportable error, failing the test with a pretty-printed
evalReportableM ::
    (MonadTest m, Show x, HasCallStack, ElaraDiagnostic e) =>
    m (Either e x) -> m x
evalReportableM m = do
    r <- m
    case r of
        Left err -> do
            let msg = diagnosticMessage err
            failWith Nothing $ toString $ prettyToText msg
        Right ok -> pure ok
