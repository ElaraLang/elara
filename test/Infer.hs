module Infer where

import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Monotype qualified as Scalar
import Elara.TypeInfer.Type (Type (..))
import Infer.Common
import Test.Hspec
import Prelude hiding (fail)

spec :: Spec
spec = describe "Infers types correctly" $ do
    simpleTypes
    functionTypes

simpleTypes :: Spec
simpleTypes = describe "Infers simple types correctly" $ do
    it "Infers Int literals correctly" $ do
        (t, fail) <- inferSpec "1" "Int"
        case t of
            Scalar () Scalar.Integer -> pass
            o -> fail o

functionTypes :: Spec
functionTypes = describe "Infers function types correctly" $ do
    it "Infers identity function correctly" $ do
        (t, fail) <- inferSpec "\\x -> x" "forall a. a -> a"
        case t of
            (Forall' a Domain.Type (Function' (VariableType' a') (VariableType' a''))) | a == a' && a == a'' -> pass
            o -> fail o

    it "Infers nested identity function correctly" $ do
        (t, fail) <- inferSpec "let id = \\x -> x in id" "forall a. a -> a"
        case t of
            (Forall' a Domain.Type (Function' (VariableType' a') (VariableType' a''))) | a == a' && a == a'' -> pass
            o -> fail o

    it "Infers VERY nested identity function correctly" $ do
        (t, fail) <- inferSpec "let id = \\x -> x in id id id id id id id id id id id" "forall a. a -> a"
        case t of
            (Forall' a Domain.Type (Function' (VariableType' a') (VariableType' a''))) | a == a' && a == a'' -> pass
            o -> fail o

    it "Infers fix-point function correctly" $ do
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

    it "Infers polymorphic lets correctly" $ do
        (t, fail) <- inferSpec "let id = \\x -> x in (id 1, id ())" "(Int, ())"
        case t of
            Tuple' (Scalar () Scalar.Integer :| [Scalar () Scalar.Unit]) -> pass
            o -> fail o
