module Parse.Common ((<:), (<=>)) where

import Control.Lens ((%~), (^.))
import Elara.AST.Frontend as AST
import Elara.Data.Module as Mod
import Elara.Data.Qualifications (MaybeQualified)
import Elara.Data.TypeAnnotation (TypeAnnotation)
import Elara.Data.Uniqueness
import Elara.Parse (parse)
import Test.Hspec.Megaparsec (parseSatisfies, shouldParse)

unlocateModule :: Module LocatedExpr p a q 'Many -> Module UnwrappedExpr p a q 'Many
unlocateModule = Mod.moduleDeclarations . traverse . Mod.declarationBody . declarationBodyExpression %~ unlocateExpr

(<:) :: Text -> Declaration UnwrappedExpr Pattern TypeAnnotation MaybeQualified -> IO ()
(<:) source decl = do
  let parsed = unlocateModule <$> parse "" source
  let matches ast = decl `elem` toList (ast ^. declarations)
  parseSatisfies parsed matches

(<=>) :: Text -> Module UnwrappedExpr Pattern TypeAnnotation MaybeQualified 'Many -> IO ()
(<=>) source expected = do
  let parsed = unlocateModule <$> parse "" source
  shouldParse parsed expected