module Parse.Names where

import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName (ModuleName), TypeName (TypeName), VarName (..))
import Elara.Parse.Names (typeName, varName)
import Elara.Parse.Primitives
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (runParser)

spec :: Spec
spec = describe "Test Names Parser" $ do
    describe "Variable names" varNames
    describe "Type names" typeNames

varNames :: Spec
varNames = do
    it "Parses a simple variable name" $ do
        "foo" <=> (notQualified $ VarName "foo", varName)
    it "Parses a variable name with a module qualification" $ do
        "Foo.bar" <=> (MaybeQualified (VarName "bar") (Just $ ModuleName ("Foo" :| [])), varName)

typeNames :: Spec
typeNames = do
    it "Parses a simple type name" $ do
        "Foo" <=> (notQualified $ TypeName "Foo", typeName)
    it "Parses a type name with a module qualification" $ do
        "Foo.Bar" <=> (MaybeQualified (TypeName "Bar") (Just $ ModuleName ("Foo" :| [])), typeName)

notQualified :: name -> MaybeQualified name
notQualified a = MaybeQualified a Nothing

(<=>) :: (Show n, Eq n) => Text -> (n, Parser n) -> IO ()
(<=>) source (expected, parser) = do
    shouldParse (runParser parser "" source) expected