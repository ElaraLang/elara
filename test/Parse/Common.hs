module Parse.Common where

import Elara.AST.Generic (Pattern, stripPatternLocation)
import Elara.AST.Select (UnlocatedAST (..))
import Elara.AST.Unlocated ()
import Elara.Lexer.Pipeline (runLexPipelinePure)
import Elara.Lexer.Reader (readTokensWith)
import Elara.Parse (parsePipeline, runParsePipelinePure)
import Elara.Parse.Error
import Elara.Parse.Pattern (patParser)
import Elara.Parse.Primitives
import Elara.Parse.Stream
import Hedgehog (MonadTest, diff, evalEither, tripping)
import Hedgehog.Internal.Property (failWith)
import Polysemy
import Print (showPretty)
import Test.QuickCheck
import Text.Megaparsec (ShowErrorComponent, TraversableStream, VisualStream, errorBundlePretty)

-- lexAndParse :: ToString a => HParser b -> a -> Either (WParseErrorBundle TokenStream ElaraParseError) b
lexAndParse :: (MonadTest m, ToString a1) => HParser a2 -> a1 -> m (Either (WParseErrorBundle TokenStream ElaraParseError) a2)
lexAndParse parser source = do
    let fp = "<tests>"
    tokens <- evalEither $ run $ runLexPipelinePure $ readTokensWith fp (toString source)
    pure $ run $ runParsePipelinePure $ parsePipeline parser fp tokens

shouldParsePattern :: MonadTest m => Text -> Pattern 'UnlocatedFrontend -> m ()
shouldParsePattern source expected = do
    parsed <- lexAndParse patParser source >>= evalEither
    diff (stripPatternLocation parsed) (==) expected

shouldFailToParse :: (MonadTest m) => Text -> m ()
shouldFailToParse source = do
    parsed <- lexAndParse patParser source
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
        Left e -> withFrozenCallStack $ failWith Nothing $ errorBundlePretty (unWParseErrorBundle e)
        Right y -> tripping x (const i) (const (Identity y))
