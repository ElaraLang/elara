module Parse.Common where

import Elara.AST.Generic (Pattern, stripPatternLocation)
import Elara.AST.Select (UnlocatedAST (..))
import Elara.AST.Unlocated.Frontend ()
import Elara.Parse.Error
import Elara.Parse.Pattern (patParser)
import Elara.Parse.Primitives
import Elara.Parse.Stream
import Hedgehog (MonadTest, diff, evalEither, success, tripping)
import Hedgehog.Internal.Property (failWith)
import Lex.Common (lex')
import Polysemy
import Polysemy.Error (Error, fromEither, runError)
import Print (showPretty)
import Test.Hspec (Expectation, expectationFailure, shouldBe)
import Test.QuickCheck
import Text.Megaparsec (ShowErrorComponent, TraversableStream, VisualStream, eof, errorBundlePretty, runParser)

lexAndParse :: Member (Error (WParseErrorBundle TokenStream ElaraParseError)) r => HParser a -> Text -> Sem r a
lexAndParse p t = fromEither (Parse.Common.parse p (TokenStream (toString t) (lex' t) 0))

parse :: HParser a -> TokenStream -> Either (WParseErrorBundle TokenStream ElaraParseError) a
parse p = first WParseErrorBundle . runParser (toParsec p <* eof) "<tests>"

shouldParsePattern :: MonadTest m => Text -> Pattern 'UnlocatedFrontend -> m ()
shouldParsePattern source expected = do
    parsed <- evalEither $ run $ runError $ lexAndParse patParser source
    diff (stripPatternLocation parsed) (==) expected

shouldFailToParse :: Text -> Expectation
shouldFailToParse source = do
    let parsed = run $ runError $ lexAndParse patParser source
    case parsed of
        Left _ -> pass
        Right ast -> expectationFailure ("Expected to fail to parse, but parsed " <> toString (showPretty ast))

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
    (b -> Either (WParseErrorBundle TokenStream ElaraParseError) a) ->
    m ()
trippingParse x encode decode =
    let
        i = encode x

        my = decode i
     in
        case my of
            Left e -> withFrozenCallStack $ failWith Nothing $ errorBundlePretty (unWParseErrorBundle e)
            Right y -> tripping x (const i) (const (Identity y))
