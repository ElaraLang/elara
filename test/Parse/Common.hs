module Parse.Common where

import Control.Lens
import Elara.AST.Frontend.StripLocation
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module (Declaration (..), Declaration', Module)
import Elara.AST.Module qualified as Mod
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.Parse (parse)
import Elara.Parse.Error (unWParseErrorBundle)
import Print (prettyShow)
import Test.Hspec.Megaparsec (parseSatisfies, shouldParse)
import Test.QuickCheck
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, TraversableStream, VisualStream, errorBundlePretty)

(<:) :: Text -> Declaration' UnlocatedFrontend -> IO ()
(<:) source decl = do
    let parsed = stripLocation <$> parse "" source
    let matches ast = Declaration decl `elem` toList (ast ^. Mod.declarations)
    parseSatisfies (first unWParseErrorBundle parsed) matches

makeMQName :: (t -> name) -> t -> Maybe ModuleName -> MaybeQualified name
makeMQName ctor n = MaybeQualified (ctor n)

shouldParseProp :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Eq a, Show a) => Either (ParseErrorBundle s e) a -> a -> Property
result `shouldParseProp` a = ioProperty $
    case result of
        Left err -> do
            pure $ counterexample (errorBundlePretty err) False
        Right ast -> if ast == a then pure $ property True else pure $ counterexample (show ast) False