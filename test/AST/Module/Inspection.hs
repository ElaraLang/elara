{-# LANGUAGE PartialTypeSignatures #-}

module AST.Module.Inspection where

import Data.List.Extra
import Data.Map qualified as M
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module (Declaration (..), DeclarationBody (..), Exposing (..), Exposition (..), Import (..), Module (..))
import Elara.AST.Module.Inspection
import Elara.AST.Name (MaybeQualified (..), ModuleName, Name (..))
import Elara.AST.Select
import Elara.Data.Type (Type (..))
import Orphans ()
import Polysemy (run)
import Polysemy.Error (runError)
import Polysemy.Reader (runReader)
import Relude.Extra.Tuple (fmapToFst)
import Test.Hspec
import Prelude hiding (runReader)

makeModule :: ModuleName -> [Declaration UnlocatedFrontend] -> [Import MaybeQualified] -> Module UnlocatedFrontend
makeModule name decls imports = Module name ExposingAll imports decls

spec :: Spec
spec = do
    describe "Module inspection works" mainTest

mainTest :: Spec
mainTest = do
    {-
            module Quux exposing (quux)
            ...
            module This exposing (bar)
            import Bar (bar)
            import Foo (fooBar)
            import Baz as B (baz, Baz)
            import Quux qualified
        the context would be
    -}
    let quux = makeModule "Quux" [Declaration "Quux" (NVarName (MaybeQualified "quux" Nothing)) (Value (Int 0) [] Nothing)] []
    let bar = makeModule "Bar" [Declaration "Bar" (NVarName (MaybeQualified "bar" Nothing)) (Value (Int 0) [] Nothing)] []
    let fooBar = makeModule "Foo" [Declaration "Foo" (NVarName (MaybeQualified "fooBar" Nothing)) (Value (Int 0) [] Nothing)] []
    let baz = makeModule "Baz" [Declaration "Baz" (NVarName (MaybeQualified "baz" Nothing)) (Value (Int 0) [] Nothing), Declaration "Baz" (NTypeName (MaybeQualified "Baz" Nothing)) (TypeAlias (UserDefinedType (MaybeQualified "Baz" Nothing)))] []
    let this =
            makeModule
                "This"
                [Declaration "This" (NVarName (MaybeQualified "bar" Nothing)) (Value (Int 0) [] Nothing)]
                [ Import "Bar" Nothing False (ExposingSome [ExposedValue (MaybeQualified "bar" Nothing)])
                , Import "Foo" Nothing False (ExposingSome [ExposedValue (MaybeQualified "fooBar" Nothing)])
                , Import "Baz" (Just "B") False (ExposingSome [ExposedValue (MaybeQualified "baz" Nothing), ExposedType (MaybeQualified "Baz" Nothing)])
                , Import "Quux" Nothing True ExposingAll
                ]
    let ctx = (run $ runError $ buildContext this (M.fromList (fmapToFst _moduleName [quux, bar, fooBar, baz, this]))) :: Either (InspectionError UnlocatedFrontend) InspectionContext
    it "Doesn't error when creating the module" $ ctx `shouldSatisfy` isRight
    let ctx' = fromRight (error "shouldn't happen") ctx
    let withCtx = (run . runReader ctx' . runError) :: _ -> Either (InspectionError UnlocatedFrontend) ModuleName
    context "When provided with unambiguous names" $ do
        it "Finds the This/Bar elements correctly" $ do
            withCtx (search (NVarName (MaybeQualified "bar" (Just "This")))) `shouldBe` Right "This"
            withCtx (search (NVarName (MaybeQualified "bar" (Just "Bar")))) `shouldBe` Right "Bar"

        it "Finds the Foo elements correctly" $ do
            withCtx (search (NVarName (MaybeQualified "fooBar" Nothing))) `shouldBe` Right "Foo"
            withCtx (search (NVarName (MaybeQualified "fooBar" (Just "Foo")))) `shouldBe` Right "Foo"

        it "Finds the Baz elements correctly" $ do
            withCtx (search (NVarName (MaybeQualified "baz" (Just "B")))) `shouldBe` Right "Baz"
            withCtx (search (NTypeName (MaybeQualified "Baz" (Just "B")))) `shouldBe` Right "Baz"
            withCtx (search (NVarName (MaybeQualified "baz" Nothing))) `shouldBe` Right "Baz"
            withCtx (search (NTypeName (MaybeQualified "Baz" Nothing))) `shouldBe` Right "Baz"

        it "Finds the Quux elements correctly" $ do
            withCtx (search (NVarName (MaybeQualified "quux" (Just "Quux")))) `shouldBe` Right "Quux"

    context "When provided with ambiguous/unqualified names" $ do
        it "Errors on This/Bar elements" $ do
            withCtx (search (NVarName (MaybeQualified "bar" Nothing))) `shouldSatisfy` \case
                Left (AmbiguousName _ possible) -> possible `isRearrangementOf` ["This", "Bar"]
                _ -> False

        it "Errors on unqualified Quux elements" $ do
            withCtx (search (NVarName (MaybeQualified "quux" Nothing))) `shouldSatisfy` \case
                Left (UnknownName _) -> True
                _ -> False

        it "Errors on Baz elements not using the alias" $ do
            withCtx (search (NVarName (MaybeQualified "baz" (Just "Baz")))) `shouldSatisfy` \case
                Left (UnknownName _) -> True
                _ -> False
            withCtx (search (NTypeName (MaybeQualified "Baz" (Just "Baz")))) `shouldSatisfy` \case
                Left (UnknownName _) -> True
                _ -> False

        it "Errors on a totally unknown name" $ do
            withCtx (search (NVarName (MaybeQualified "unknown" Nothing))) `shouldSatisfy` \case
                Left (UnknownName _) -> True
                _ -> False