module Parse.Common where

import Control.Lens
import Elara.AST.Frontend.StripLocation

import Elara.AST.Module qualified as Mod
import Elara.Parse.Primitives (HParser)
import Elara.Lexer.Lexer (lex)
import Elara.AST.Name
import Elara.AST.Frontend.Unlocated as UnlocatedFrontend
import Elara.AST.Select
import Elara.Parse (parse)
import Elara.Parse.Error (unWParseErrorBundle)
import Test.Hspec.Megaparsec (parseSatisfies)
import Test.QuickCheck
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, TraversableStream, VisualStream, errorBundlePretty)
import Elara.Parse.Stream


(<:) :: Text -> UnlocatedFrontend.Declaration -> IO ()
(<:) source decl = do
    let tokens = either (error . show) id $ lex "" (encodeUtf8 source)
    let parsed = stripLocation <$> parse "" (TokenStream (toString source) tokens)
    let matches ast = decl `elem` toList (ast ^. Mod.declarations)
    parseSatisfies (first unWParseErrorBundle parsed) matches

makeMQName :: (t -> name) -> t -> Maybe ModuleName -> MaybeQualified name
makeMQName ctor n = MaybeQualified (ctor n)

shouldParseProp :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Eq a, Show a) => Either (ParseErrorBundle s e) a -> a -> Property
result `shouldParseProp` a = ioProperty $
    case result of
        Left err -> do
            pure $ counterexample (errorBundlePretty err) False
        Right ast -> if ast == a then pure $ property True else pure $ counterexample (show ast) False