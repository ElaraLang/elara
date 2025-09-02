module Parse.Common where

import Elara.AST.Generic (Expr, Pattern)
import Elara.AST.Select (UnlocatedAST (..))
import Elara.AST.StripLocation
import Elara.AST.Unlocated ()
import Elara.Data.Pretty (pretty, (<+>))
import Elara.Lexer.Pipeline (runLexPipelinePure)
import Elara.Lexer.Reader (readTokensWith)
import Elara.Parse (parsePipeline, runParsePipelinePure)
import Elara.Parse.Error
import Elara.Parse.Expression (element, exprParser)
import Elara.Parse.Indents
import Elara.Parse.Pattern (patParser)
import Elara.Parse.Primitives
import Elara.Parse.Stream
import Hedgehog (MonadTest, diff, evalEither, footnoteShow, tripping)
import Hedgehog.Internal.Property (failWith)
import Polysemy
import Print (printPretty, showPretty)
import Test.QuickCheck
import Text.Megaparsec (ShowErrorComponent, TraversableStream, VisualStream, eof, errorBundlePretty)

evalEitherParseError :: (ShowErrorComponent e, VisualStream s, TraversableStream s, MonadTest m) => Either (WParseErrorBundle s e) a -> m a
evalEitherParseError = withFrozenCallStack $ either (failWith Nothing . errorBundlePretty . unWParseErrorBundle) pure

lexAndParse :: (MonadTest m, ToString a1) => Parser a2 -> a1 -> m (Either (WParseErrorBundle TokenStream ElaraParseError) a2)
lexAndParse parser source = do
    let fp = "<tests>"
    tokens <- evalEither $ run $ runLexPipelinePure $ readTokensWith fp (toString source)
    pure $ run $ runParsePipelinePure $ parsePipeline parser fp (toString source, tokens)

shouldParsePattern :: MonadTest m => Text -> Pattern 'UnlocatedFrontend -> m ()
shouldParsePattern source expected = withFrozenCallStack $ do
    parsed <- lexAndParse patParser source >>= evalEitherParseError
    diff (stripLocation parsed) (==) expected

shouldParseExpr :: MonadTest m => Text -> Expr 'UnlocatedFrontend -> m ()
shouldParseExpr source expected = withFrozenCallStack $ do
    parsed <- lexAndParse (exprBlock element) source >>= evalEitherParseError
    diff (stripLocation parsed) (==) expected

shouldFailToParse :: MonadTest m => Text -> m ()
shouldFailToParse source = withFrozenCallStack $ do
    parsed <- lexAndParse (patParser <* eof) source
    case parsed of
        Left _ -> pass
        Right ast -> failWith Nothing ("Expected to fail to parse, but parsed " <> toString (showPretty ast))

shouldParseProp :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Eq a, Show a) => Either (WParseErrorBundle s e) a -> a -> Property
result `shouldParseProp` a = ioProperty $
    case result of
        Left err -> do
            pure $ counterexample (errorBundlePretty (unWParseErrorBundle err)) False
        Right ast -> if ast == a then pure $ property True else pure $ counterexample (show ast) False

trippingParse ::
    (MonadTest m, Show b, HasCallStack, Eq a, Show a) =>
    a ->
    (a -> b) ->
    (b -> m (Either (WParseErrorBundle TokenStream ElaraParseError) a)) ->
    m ()
trippingParse x encode decode = do
    let i = encode x
    my <- decode i
    case my of
        Left e -> do
            footnoteShow i
            withFrozenCallStack $ failWith Nothing $ errorBundlePretty (unWParseErrorBundle e)
        Right y -> withFrozenCallStack $ tripping x (const i) (const (Identity y))
