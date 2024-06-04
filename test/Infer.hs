module Infer where

import Arbitrary.Type (arbitraryType)
import Common (diagShouldSucceed, runUnique)
import Data.Generics.Product (HasField (field))
import Data.Generics.Sum (AsConstructor' (_Ctor'))
import Data.Generics.Wrapped (_Unwrapped)
import Elara.AST.Generic.Types
import Elara.AST.Name
import Elara.AST.Region (unlocated)
import Elara.AST.Select (LocatedAST (..))
import Elara.AST.StripLocation
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Monotype qualified as Scalar
import Elara.TypeInfer.Type (applicableTyApp)
import Elara.TypeInfer.Type as Type (Type (..), structuralEq)
import Elara.TypeInfer.Unique (makeUniqueTyVar)
import Infer.Common
import Print (printPretty)
import Relude.Unsafe ((!!))
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude hiding (fail)

spec :: Spec
spec = describe "Infers types correctly" $ parallel $ do
    simpleTypes
    functionTypes
    typeApplications

simpleTypes :: Spec
simpleTypes = describe "Infers simple types correctly" $ parallel $ do
    it "Infers Int literals correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "1" "Int"
        case t of
            Scalar () Scalar.Integer -> pass
            o -> fail o

    it "Infers Unit literals correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "()" "()"
        case t of
            Scalar () Scalar.Unit -> pass
            o -> fail o

    it "Infers Real literals correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "1.0" "Real"
        case t of
            Scalar () Scalar.Real -> pass
            o -> fail o

    it "Infers Text literals correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "\"hello\"" "Text"
        case t of
            Scalar () Scalar.Text -> pass
            o -> fail o

    it "Infers Char literals correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "'c'" "Text"
        case t of
            Scalar () Scalar.Char -> pass
            o -> fail o

functionTypes :: Spec
functionTypes = describe "Infers function types correctly" $ modifyMaxSuccess (const 1) $ do
    it "Infers identity function correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "\\x -> x" "forall a. a -> a"
        case t of
            (Forall' a Domain.Type (Function' (VariableType' a') (VariableType' a''))) | a == a' && a == a'' -> pass
            o -> fail o

    it "Infers nested identity function correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "let id = \\x -> x in id" "forall a. a -> a"
        case t of
            (Forall' a Domain.Type (Function' (VariableType' a') (VariableType' a''))) | a == a' && a == a'' -> pass
            o -> fail o

    it "Infers VERY nested identity function correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "let id = \\x -> x in id id id id id id id id id id id" "forall a. a -> a"
        case t of
            (Forall' a Domain.Type (Function' (VariableType' a') (VariableType' a''))) | a == a' && a == a'' -> pass
            o -> fail o

    it "Infers fix-point function correctly" $ hedgehog $ do
        (t, fail) <- inferSpec "let fix = \\f -> f (fix f) in fix" "forall a b. (a -> b) -> b"
        case t of
            Forall'
                a
                Domain.Type
                ( Forall'
                        b
                        Domain.Type
                        (Function' (Function' (VariableType' a') (VariableType' b')) (VariableType' b''))
                    ) | a == a' && b == b' && b == b'' -> pass
            o -> fail o

    it "Infers polymorphic lets correctly" $ hedgehog $ do
        (mod, _) <-
            liftIO
                ( inferModuleFully @Text
                    "type Pair a b = Pair a b; let p = let id = \\x -> x in Pair (id 1) (id ())"
                )
                >>= diagShouldSucceed
        let decls = modDecls mod
        let pDecl = decls !! 0 ^. _Unwrapped % _2 % to stripLocation
        case pDecl of
            Type.Custom () (Qualified "Pair" "Main") [Scalar () Scalar.Integer, Scalar () Scalar.Unit] -> pass
            o -> failTypeMismatch "p" "Pair Int ()" o

    it "Correctly adds type applications when referring to another polymorphic function" $ hedgehog $ do
        (mod, _) <-
            liftIO
                ( inferModuleFully @Text
                    "let id_ x = x; let id = id_; def id2 : Int -> Int; let id2 = id_; def id3: (a -> a) -> (a -> a); let id3 = id_;"
                )
                >>= diagShouldSucceed
        let decls = modDecls mod

        let idDecl = decls !! 1 ^. _Unwrapped % _1 % unlocated
        let id2Decl = decls !! 2 ^. _Unwrapped % _1 % unlocated
        let id3Decl = decls !! 3 ^. _Unwrapped % _1 % unlocated

        printPretty decls

        case idDecl of
            (Elara.AST.Generic.Types.Var _) -> pass
            o -> failTypeMismatch "id" "Main.id @a" o

        case id2Decl of
            (TypeApplication _ (Scalar{scalar = Scalar.Integer})) -> pass
            o -> failTypeMismatch "id" "Main.id @Int" o

        case id3Decl of
            (TypeApplication _ (Function _ a b)) | a `structuralEq` b -> pass
            o -> failTypeMismatch "id" "Main.id @(a -> a)" o

typeApplications :: Spec
typeApplications = describe "Correctly determines which type applications to add" $ do
    it "Doesn't add unnecessary ty-apps with monotypes" $ hedgehog $ do
        Scalar () Scalar.Integer `applicableTyApp` Scalar () Scalar.Integer === []
        Scalar () Scalar.Integer `applicableTyApp` Scalar () Scalar.Char === []

    it "Doesn't add unnecessary ty-apps with polymorphic types" $ hedgehog $ do
        n <- runUnique makeUniqueTyVar
        Forall () () n Domain.Type (Scalar () Scalar.Char) `applicableTyApp` Scalar () Scalar.Integer === []

    it "Adds ty-apps to forall a. a" $ hedgehog $ do
        n <- runUnique makeUniqueTyVar
        someT <- forAll arbitraryType
        Forall () () n Domain.Type (VariableType () n) `applicableTyApp` someT === [someT]

    it "Adds ty-apps to forall a. a -> a" $ hedgehog $ do
        n <- runUnique makeUniqueTyVar
        someT <- forAll arbitraryType
        let forAllT = Forall () () n Domain.Type (Function () (VariableType () n) (VariableType () n))

        -- forall `applicableTyApp` someT === [] -- usages like this should really throw an error
        forAllT `applicableTyApp` Function () someT someT === [someT]

        -- instantiating forall a. a -> a to forall b. (b -> b) -> b -> b should give us [b -> b]
        n2 <- runUnique makeUniqueTyVar
        let n2Var = VariableType () n2
        forAllT `applicableTyApp` Forall () () n2 Domain.Type (Function () (Function () n2Var n2Var) (Function () n2Var n2Var)) === [Function () n2Var n2Var]

modDecls mod =
    mod
        ^.. _Unwrapped
        % unlocated
        % field @"declarations"
        % folded
        % _Unwrapped
        % unlocated
        % field @"body"
        % _Unwrapped
        % unlocated
        % (_Ctor' @"Value" @(DeclarationBody' Typed))
        % _1
