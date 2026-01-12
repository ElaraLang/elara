module ConstExpr where

import Effectful
import Elara.AST.Generic (AnnotationArg (..), Expr (..), Expr' (..))
import Elara.AST.Unlocated ()
import Elara.ConstExpr (ConstVal (..), interpretAnnotationArg)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Region (testLocated)
import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "ConstExpr" $ do
    intTests
    stringTests
    charTests
    unitTests

-- Helper to create an annotation arg
mkAnnotationArg :: Expr' ast -> AnnotationArg ast
mkAnnotationArg expr = AnnotationArg (Expr (testLocated expr, Nothing))

-- Helper to run the interpreter
runInterp :: AnnotationArg ast -> IO ConstVal
runInterp = runEff . interpretAnnotationArg

intTests :: Spec
intTests = describe "interprets integer constants" $ do
    it "interprets positive integers" $ property $ do
        n <- forAll $ Gen.integral (Range.linear 0 1000)
        result <- liftIO $ runInterp (mkAnnotationArg (Int n))
        result === ConstInt n

    it "interprets negative integers" $ property $ do
        n <- forAll $ Gen.integral (Range.linear (-1000) 0)
        result <- liftIO $ runInterp (mkAnnotationArg (Int n))
        result === ConstInt n

    it "interprets zero" $ do
        result <- runInterp (mkAnnotationArg (Int 0))
        result `shouldBe` ConstInt 0

stringTests :: Spec
stringTests = describe "interprets string constants" $ do
    it "interprets arbitrary strings" $ property $ do
        s <- forAll $ Gen.text (Range.linear 0 20) Gen.unicode
        result <- liftIO $ runInterp (mkAnnotationArg (String s))
        result === ConstString s

    it "interprets empty strings" $ do
        result <- runInterp (mkAnnotationArg (String ""))
        result `shouldBe` ConstString ""

    it "interprets strings with special characters" $ do
        result <- runInterp (mkAnnotationArg (String "hello\nworld\t!"))
        result `shouldBe` ConstString "hello\nworld\t!"

charTests :: Spec
charTests = describe "interprets char constants" $ do
    it "interprets arbitrary characters" $ property $ do
        c <- forAll Gen.unicode
        result <- liftIO $ runInterp (mkAnnotationArg (Char c))
        result === ConstChar c

    it "interprets special characters" $ do
        result <- runInterp (mkAnnotationArg (Char '\n'))
        result `shouldBe` ConstChar '\n'

unitTests :: Spec
unitTests = describe "interprets unit constants" $ do
    it "interprets unit" $ do
        result <- runInterp (mkAnnotationArg Unit)
        result `shouldBe` ConstUnit
