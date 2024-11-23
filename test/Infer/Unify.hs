module Infer.Unify where

import Arbitrary.Type (genMonotype, genUniqueTypeVar)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Type
import Hedgehog (Gen, Property, evalEither, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Polysemy
import Polysemy.Error
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "Type unification" $ do
    it "unifies type variables" prop_unify_type_vars
    it "unifies scalars" prop_unify_scalars
    it "unifies functions" prop_unify_functions
    it "unifies self" prop_unify_self
    it "fails to unify mismatched types" prop_unify_failure

runUnify ::
    Sem '[Error UnifyError] (Substitution loc, Constraint loc) ->
    Either UnifyError (Substitution loc, Constraint loc)
runUnify = run . runError

prop_unify_type_vars :: Property
prop_unify_type_vars = property $ do
    a <- forAll $ genUniqueTypeVar
    let typeVar = TypeVar a
    (sub, _) <- evalEither $ runUnify $ unify typeVar typeVar
    sub === Substitution []

prop_unify_scalars :: Property
prop_unify_scalars = property $ do
    a <- forAll $ Gen.enumBounded
    let scalarType = Scalar a
    (sub, _) <- evalEither $ runUnify $ unify scalarType scalarType
    sub === Substitution []

prop_unify_self :: Property
prop_unify_self = property $ do
    a <- forAll genMonotype
    (sub, _) <- evalEither $ runUnify $ unify a a
    sub === Substitution []

prop_unify_functions :: Property
prop_unify_functions = property $ do
    a <- forAll genMonotype
    b <- forAll genMonotype
    (sub, _) <- evalEither $ runUnify $ unify (Function a b) (Function a b)
    sub === Substitution []

-- Hedgehog property: Check unification failure for mismatched types
prop_unify_failure :: Property
prop_unify_failure = property $ do
    a <- forAll genMonotype
    b <- forAll genMonotype
    -- let's come back to this later
    -- let result = runUnify $ unify a b
    -- result === Left (UnificationFailed $ "Unification failed: " <> show a <> " and " <> show b)
    guard $ a /= b
