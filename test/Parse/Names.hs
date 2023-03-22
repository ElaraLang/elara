module Parse.Names where

import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName (ModuleName), TypeName (TypeName), VarName (..))
import Elara.Parse.Names (typeName, varName)
import Elara.Parse.Primitives
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (runParser)
import Elara.Lexer.Lexer (lex)
import Elara.Parse.Stream (TokenStream(..))

spec :: Spec
spec = describe "Test Names Parser" $ do
    describe "Variable names" varNames
    describe "Type names" typeNames

varNames :: Spec
varNames = do
    it "Parses a simple variable name" $ do
        "foo" <=> (notQualified $ NormalVarName "foo", varName)
    it "Parses a variable name with a module qualification" $ do
        "Foo.bar" <=> (MaybeQualified (NormalVarName "bar") (Just $ ModuleName ("Foo" :| [])), varName)

typeNames :: Spec
typeNames = do
    it "Parses a simple type name" $ do
        "Foo" <=> (notQualified $ TypeName "Foo", typeName)
    it "Parses a type name with a module qualification" $ do
        "Foo.Bar" <=> (MaybeQualified (TypeName "Bar") (Just $ ModuleName ("Foo" :| [])), typeName)

notQualified :: name -> MaybeQualified name
notQualified a = MaybeQualified a Nothing

(<=>) :: (Show n, Eq n) => Text -> (n, HParser n) -> IO ()
(<=>) source (expected, parser) = do
    let tokens = fromRight (error "lex error") $ lex "" (encodeUtf8 source)
    shouldParse (runParser (toParsec parser) "" (TokenStream (toString source) tokens)) expected