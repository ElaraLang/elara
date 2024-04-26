module Infer where

import Common (diagShouldSucceed)
import Control.Lens (Each (each), Field1 (_1), Prism', filtered, foldOf, folded, mapped, preview, traverseOf, (^.), (^..), (^?))
import Data.Generics.Product (HasField (field))
import Data.Generics.Sum (AsAny (_As), AsConstructor (_Ctor), AsConstructor' (_Ctor'), AsType (_Typed))
import Data.Generics.Wrapped (_Unwrapped)
import Data.Map ((!))
import Elara.AST.Generic.Common
import Elara.AST.Generic.Types
import Elara.AST.Region (unlocated)
import Elara.AST.Select (LocatedAST (..), UnlocatedAST (UnlocatedTyped))
import Elara.AST.StripLocation
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Monotype qualified as Scalar
import Elara.TypeInfer.Type (Type (..))
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
        (t, fail) <- inferSpec "let id = \\x -> x in (id 1, id ())" "(Int, ())"
        case t of
            Tuple' (Scalar () Scalar.Integer :| [Scalar () Scalar.Unit]) -> pass
            o -> fail o

    it "Correctly adds type applications when referring to another polymorphic function" $ hedgehog $ do
        (mod, _) <- liftIO (inferModuleFully @Text "let id_ x = x; let id = id_; def id2 : Int -> Int; let id2 = id_") >>= diagShouldSucceed
        let decls =
                mod
                    ^.. _Unwrapped
                        . unlocated
                        . field @"declarations"
                        . folded
                        . _Unwrapped
                        . unlocated
                        . field @"body"
                        . _Unwrapped
                        . unlocated
                        . (_Ctor' @"Value" @(DeclarationBody' Typed))
                        . _1

        let idDecl = decls !! 1 ^. _Unwrapped . _1 . unlocated
        let id2Decl = decls !! 2 ^. _Unwrapped . _1 . unlocated

        printPretty decls

        case idDecl of
            (Elara.AST.Generic.Types.Var _) -> pass
            o -> failTypeMismatch "id" "Main.id @a" o

        case id2Decl of
            (TypeApplication x y) -> pass
            o -> failTypeMismatch "id" "Main.id @Int" o
