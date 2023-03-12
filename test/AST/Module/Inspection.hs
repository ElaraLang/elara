{-# LANGUAGE PartialTypeSignatures #-}

module AST.Module.Inspection where

import Control.Lens
import Data.List.Extra
import Data.Map qualified as M
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module 
import Elara.AST.Module.Inspection
import Elara.AST.Name (MaybeQualified (..), ModuleName, Name (..))
import Elara.AST.Select
import Orphans ()
import Polysemy (run)
import Polysemy.Error (runError)
import Polysemy.Reader (runReader)
import Relude.Extra.Tuple (fmapToFst)
import Test.Hspec
import Prelude hiding (runReader)
import Parse.Common

makeModule :: ModuleName -> [Declaration' UnlocatedFrontend] -> [Import' UnlocatedFrontend] -> Module UnlocatedFrontend
makeModule name decls imports = Module (Module' name ExposingAll (Import <$> imports) (Declaration <$> decls))

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
    let quux = makeModule "Quux" [Declaration' "Quux" (makeMQName NVarName "quux" Nothing) (DeclarationBody $ (Value (Int 0) [] Nothing))] []
    let bar = makeModule "Bar" [Declaration' "Bar" (makeMQName NVarName "bar" Nothing) (DeclarationBody $ Value (Int 0) [] Nothing)] []
    let fooBar = makeModule "Foo" [Declaration' "Foo" (makeMQName NVarName "fooBar" Nothing) (DeclarationBody $ Value (Int 0) [] Nothing)] []
    let baz = makeModule "Baz" [
                Declaration' "Baz" (makeMQName NVarName "baz" Nothing) (DeclarationBody (Value (Int 0) [] Nothing)), 
                Declaration' "Baz" (makeMQName NTypeName "Baz" Nothing) (DeclarationBody (TypeAlias (UserDefinedType (MaybeQualified "Baz" Nothing))))
            ] []
    let this =
            makeModule
                "This"
                [Declaration' "This" (makeMQName NVarName "bar" Nothing) (DeclarationBody $ Value (Int 0) [] Nothing)]
                [ Import' "Bar" Nothing False (ExposingSome [ExposedValue (MaybeQualified "bar" Nothing)])
                , Import' "Foo" Nothing False (ExposingSome [ExposedValue (MaybeQualified "fooBar" Nothing)])
                , Import' "Baz" (Just "B") False (ExposingSome [ExposedValue (MaybeQualified "baz" Nothing), ExposedType (MaybeQualified "Baz" Nothing)])
                , Import' "Quux" Nothing True ExposingAll
                ]
    let ctx = (run $ runError $ buildContext this (M.fromList (fmapToFst (view name) [quux, bar, fooBar, baz, this]))) :: Either (ContextBuildingError UnlocatedFrontend) InspectionContext
    it "Doesn't error when creating the module" $ ctx `shouldSatisfy` isRight
    let ctx' = fromRight (error "shouldn't happen") ctx
    
    let withCtx = (run . runReader ctx' . runError) :: _ -> Either (InspectionError UnlocatedFrontend) ModuleName
    context "When provided with unambiguous names" $ do
        it "Finds the This/Bar elements correctly" $ do
            withCtx (search (makeMQName NVarName "bar" (Just "This"))) `shouldBe` Right "This"
            withCtx (search (makeMQName NVarName "bar" (Just "Bar"))) `shouldBe` Right "Bar"

        it "Finds the Foo elements correctly" $ do
            withCtx (search (makeMQName NVarName "fooBar" Nothing)) `shouldBe` Right "Foo"
            withCtx (search (makeMQName NVarName "fooBar" (Just "Foo"))) `shouldBe` Right "Foo"

        it "Finds the Baz elements correctly" $ do
            withCtx (search (makeMQName NVarName "baz" (Just "B"))) `shouldBe` Right "Baz"
            withCtx (search (makeMQName NTypeName "Baz" (Just "B"))) `shouldBe` Right "Baz"
            withCtx (search (makeMQName NVarName "baz" Nothing)) `shouldBe` Right "Baz"
            withCtx (search (makeMQName NTypeName "Baz" Nothing)) `shouldBe` Right "Baz"

        it "Finds the Quux elements correctly" $ do
            withCtx (search (makeMQName NVarName "quux" (Just "Quux"))) `shouldBe` Right "Quux"

    context "When provided with ambiguous/unqualified names" $ do
        it "Errors on This/Bar elements" $ do
            withCtx (search (makeMQName NVarName "bar" Nothing)) `shouldSatisfy` \case
                Left (AmbiguousName _ possible) -> possible `isRearrangementOf` ["This", "Bar"]
                _ -> False

        it "Errors on unqualified Quux elements" $ do
            withCtx (search (makeMQName NVarName "quux" Nothing)) `shouldSatisfy` \case
                Left (UnknownName _) -> True
                _ -> False

        it "Errors on Baz elements not using the alias" $ do
            withCtx (search (makeMQName NVarName "baz" (Just "Baz"))) `shouldSatisfy` \case
                Left (UnknownName _) -> True
                _ -> False
            withCtx (search (makeMQName NTypeName "Baz" (Just "Baz"))) `shouldSatisfy` \case
                Left (UnknownName _) -> True
                _ -> False

        it "Errors on a totally unknown name" $ do
            withCtx (search (makeMQName NVarName "unknown" Nothing)) `shouldSatisfy` \case
                Left (UnknownName _) -> True
                _ -> False