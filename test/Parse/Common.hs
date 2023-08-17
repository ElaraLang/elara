module Parse.Common where

import Elara.AST.Generic (Pattern, stripPatternLocation)
import Elara.AST.Select (UnlocatedAST (..))
import Elara.AST.Unlocated.Frontend ()
import Elara.Parse.Error
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives
import Elara.Parse.Stream
import Lex.Common (lex')
import Polysemy
import Polysemy.Error (Error, fromEither, runError)
import Test.Hspec (Expectation, expectationFailure, shouldBe)
import Test.QuickCheck
import Text.Megaparsec (ShowErrorComponent, TraversableStream, VisualStream, eof, errorBundlePretty, runParser)

lexAndParse :: Member (Error (WParseErrorBundle TokenStream ElaraParseError)) r => HParser a -> Text -> Sem r a
lexAndParse p t = fromEither (Parse.Common.parse p (TokenStream (toString t) (lex' t) 0))

parse :: HParser a -> TokenStream -> Either (WParseErrorBundle TokenStream ElaraParseError) a
parse p = first WParseErrorBundle . runParser (toParsec p <* eof) "<tests>"

shouldParsePattern :: Text -> Pattern 'UnlocatedFrontend -> Expectation
shouldParsePattern source expected = do
    let parsed = run $ runError $ lexAndParse pattern' source
    case parsed of
        Left err -> expectationFailure (errorBundlePretty (unWParseErrorBundle err))
        Right ast -> stripPatternLocation ast `shouldBe` expected

shouldParseProp :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Eq a, Show a) => Either (WParseErrorBundle s e) a -> a -> Property
result `shouldParseProp` a = ioProperty $
    case result of
        Left err -> do
            pure $ counterexample (errorBundlePretty (unWParseErrorBundle err)) False
        Right ast -> if ast == a then pure $ property True else pure $ counterexample (show ast) False
