module Parse.Names where

import Elara.Data.Name (ModuleName (ModuleName), Name (Name, Qualified), NameLike, QualifiedName (QualifiedName))
import Elara.Parse.Name (moduleName, typeName, varName)
import Elara.Parse.Primitives
import Parse.Common (testParse')
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)

spec :: Spec
spec = describe "Test Names Parser" $ do
  it "Should parse a simple type name correctly" $ do
    "Int" <=> (Name "Int", typeName)

  it "Should parse a simple qualified type name correctly" $ do
    "A.Mod.Int" <=> (Qualified (QualifiedName (ModuleName ["A", "Mod"]) (Name "Int")), typeName)

  it "Should parse a simple variable name correctly" $ do
    "x" <=> (Name "x", varName)

  it "Should parse a simple qualified variable name correctly" $ do
    "A.Mod.x" <=> (Qualified (QualifiedName (ModuleName ["A", "Mod"]) (Name "x")), varName)

  it "Should parse a module name correctly" $ do
    "Elara.Parse.Names" <=> (ModuleName ["Elara", "Parse", "Names"], moduleName)

(<=>) :: (NameLike n, Show n, Eq n) => Text -> (n, Parser n) -> IO ()
(<=>) source (expected, parser) = do
  ast <- testParse' source parser
  ast `shouldBe` expected