-- | Common parsing test utilities
module Parse.Common (
    -- * Parser Runners
    parseWith,
    lexAndParse,

    -- * Test Assertions
    evalEitherParseError,
    shouldParsePattern,
    shouldParseExpr,
    shouldFailToParse,
    shouldParseProp,
    trippingParse,
) where

import Effectful (runPureEff)
import Effectful.Error.Static (runError)
import Elara.AST.Generic (Expr, Pattern)
import Elara.AST.Select (UnlocatedAST (..))
import Elara.AST.StripLocation (StripLocation (..))
import Elara.AST.Unlocated ()
import Elara.Lexer.Reader (readTokensWith)
import Elara.Logging (ignoreStructuredDebug)
import Elara.Parse.Error (ElaraParseError, WParseErrorBundle (..), unWParseErrorBundle)
import Elara.Parse.Expression (element)
import Elara.Parse.Indents (exprBlock)
import Elara.Parse.Pattern (patParser)
import Elara.Parse.Primitives (Parser)
import Elara.Parse.Stream (TokenStream (..))
import Elara.ReadFile (FileContents (..))
import Hedgehog (MonadTest, diff, evalEither, footnoteShow, tripping)
import Hedgehog.Internal.Property (failWith)
import Print (showPretty)
import Test.QuickCheck (Property, counterexample, ioProperty, property)
import Text.Megaparsec (ShowErrorComponent, TraversableStream, VisualStream, eof, errorBundlePretty, runParserT)

-- | Evaluate an 'Either' containing a parse error, failing the test if it's an error
evalEitherParseError ::
    (ShowErrorComponent e, VisualStream s, TraversableStream s, MonadTest m) =>
    Either (WParseErrorBundle s e) a -> m a
evalEitherParseError = withFrozenCallStack $ either (failWith Nothing . errorBundlePretty . unWParseErrorBundle) pure

-- | Helper to run a parser for testing purposes
parseWith ::
    Parser a ->
    -- | Filename for error messages
    FilePath ->
    -- | Source code (currently ignored)
    Text ->
    -- | Token stream to parse
    TokenStream ->
    Either (WParseErrorBundle TokenStream ElaraParseError) a
parseWith parser fp _source tokenStream =
    first WParseErrorBundle $ runPureEff $ ignoreStructuredDebug $ runParserT parser fp tokenStream

-- | Lex and parse some source code for testing purposes
lexAndParse ::
    (MonadTest m, ToText a1) =>
    -- | Parser to use
    Parser a2 ->
    -- | Source code to lex and parse
    a1 ->
    m (Either (WParseErrorBundle TokenStream ElaraParseError) a2)
lexAndParse parser source = do
    let fp = "<tests>"
    tokens <- evalEither $ runPureEff $ runError $ ignoreStructuredDebug $ readTokensWith (FileContents fp (toText source))
    let tokenStream = TokenStream (toText source) tokens False
    pure $ parseWith parser fp (toText source) tokenStream

shouldParsePattern :: MonadTest m => Text -> Pattern UnlocatedFrontend -> m ()
shouldParsePattern source expected = withFrozenCallStack $ do
    parsed <- lexAndParse patParser source >>= evalEitherParseError
    let stripped :: Pattern UnlocatedFrontend = stripLocation parsed
    diff stripped (==) expected

shouldParseExpr :: MonadTest m => Text -> Expr UnlocatedFrontend -> m ()
shouldParseExpr source expected = withFrozenCallStack $ do
    parsed <- lexAndParse (exprBlock element) source >>= evalEitherParseError
    let stripped :: Expr UnlocatedFrontend = stripLocation parsed
    diff stripped (==) expected

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
