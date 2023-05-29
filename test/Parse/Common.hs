module Parse.Common where

import Control.Lens
import Elara.AST.Name
import Elara.AST.Select
import Elara.Parse (parse)
import Elara.Parse.Error
import Elara.Parse.Primitives
import Elara.Parse.Stream
import Test.Hspec.Megaparsec (parseSatisfies)
import Test.QuickCheck
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, TraversableStream, VisualStream, eof, errorBundlePretty, runParser)

parse :: HParser a -> TokenStream -> Either (ParseErrorBundle TokenStream ElaraParseError) a
parse p = runParser (toParsec p <* eof) "<tests>"

shouldParseProp :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Eq a, Show a) => Either (ParseErrorBundle s e) a -> a -> Property
result `shouldParseProp` a = ioProperty $
    case result of
        Left err -> do
            pure $ counterexample (errorBundlePretty err) False
        Right ast -> if ast == a then pure $ property True else pure $ counterexample (show ast) False
