module Parse.Common where

import Control.Lens
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module (Declaration, Module)
import Elara.AST.Module qualified as Mod
import Elara.AST.Select
import Elara.Parse (parse)
import Test.Hspec.Megaparsec (parseSatisfies, shouldParse)
import Test.QuickCheck
import Text.Megaparsec (ParseErrorBundle, ShowErrorComponent, TraversableStream, VisualStream, errorBundlePretty)

unlocateModule :: Module Frontend -> Module UnlocatedFrontend
unlocateModule = Mod.moduleDeclarations . traverse . Mod._declarationBodyLens . Mod._declarationBodyExpressionLens %~ stripLocation

(<:) :: Text -> Declaration UnlocatedFrontend -> IO ()
(<:) source decl = do
    let parsed = unlocateModule <$> parse "" source
    let matches ast = decl `elem` toList (ast ^. Mod.declarations)
    parseSatisfies parsed matches

shouldParseProp :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Eq a) => Either (ParseErrorBundle s e) a -> a -> Property
result `shouldParseProp` a = ioProperty $
    case result of
        Left err -> do
            pure $ counterexample (errorBundlePretty err) False
        Right ast -> pure $ property (ast == a)