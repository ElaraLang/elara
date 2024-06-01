module Common where

import Elara.AST.Generic
import Elara.Data.Pretty
import Elara.Data.Unique (UniqueGen, uniqueGenToIO)
import Error.Diagnose (Diagnostic, TabSize (..), WithUnicode (..), hasReports, prettyDiagnostic')
import Hedgehog.Internal.Property
import Orphans ()
import Polysemy (Sem, runM)
import Polysemy.Embed
import Test.HUnit (assertFailure)
import Test.Hspec

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

runUnique :: MonadIO m => Sem [UniqueGen, Embed IO] a -> m a
runUnique = liftIO . runM @IO . uniqueGenToIO
