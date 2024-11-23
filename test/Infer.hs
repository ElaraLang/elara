module Infer where

import Boilerplate (loadShuntedExpr, pipelineResShouldSucceed)
import Elara.AST.Generic.Types (Expr (..), Expr' (..))
import Elara.AST.Module
import Elara.AST.Region (Located (Located), generatedSourceRegion)
import Elara.AST.Select (LocatedAST (Shunted))
import Elara.AST.Shunted
import Elara.AST.VarRef
import Elara.Data.Unique (uniqueGenToIO)
import Elara.Pipeline
import Elara.Prim (primRegion)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Type
import Hedgehog (Property, assert, evalEither, evalEitherM, evalIO, failure, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Optics.Operators.Unsafe ((^?!))
import Polysemy (Sem, run, runM, subsume, subsume_)
import Polysemy.Error (runError)
import Polysemy.Writer (runWriter)
import Print (showColored, showPretty)
import Region (qualifiedTest, testLocated)
import Relude.Unsafe ((!!))
import Test.Syd
import Test.Syd.Expectation
import Test.Syd.Hedgehog ()
import Prelude hiding (fail)

spec :: Spec
spec = describe "Infers types correctly" $ parallel $ do
    literalTests
    lambdaTests

    it "infers literals" prop_literalTypesInvariants

-- Literal Type Inference Tests
literalTests :: Spec
literalTests = describe "Literal Type Inference" $ do
    it "infers Int type correctly" $ do
        result <- runInfer $ generateConstraints emptyTypeEnvironment (mkIntExpr 42)
        result `shouldSucceed` \(constraints, ty) -> do
            constraints `shouldBe` mempty
            ty `shouldSatisfy` isScalarInt

    it "infers Float type correctly" $ do
        result <- runInfer $ generateConstraints emptyTypeEnvironment (mkFloatExpr 42.0)
        result `shouldSucceed` \(constraints, ty) -> do
            constraints `shouldBe` mempty
            ty `shouldSatisfy` isScalarFloat

lambdaTests :: Spec
lambdaTests = describe "Lambda Type Inference" $ do
    it "infers lambda type correctly" $ do
        let expr = loadShuntedExpr "\\x -> x"
        res <- pipelineResShouldSucceed expr
        result <- liftIO $ runInfer $ generateConstraints emptyTypeEnvironment res

        result `shouldSucceed` \(constraint, ty) -> do
            (tv1, tv2) <- case constraint of
                Equality tv1 tv2 -> pure (tv1, tv2)
                _ -> expectationFailure $ "Expected equality constraint, got: " ++ show constraint
            case ty of
                Function a b | a == tv1 && b == tv2 -> pass
                _ -> expectationFailure $ "Expected function type, got: " ++ show ty

    it "infers applied identity function correctly" $ do
        let expr = loadShuntedExpr "(\\x -> x) 42"
        res <- pipelineResShouldSucceed expr
        result <- liftIO $ runInfer $ generateConstraints emptyTypeEnvironment res

        result `shouldSucceed` \(constraint, ty) -> do
            (tv1, tv2) <- case constraint of
                Conjunction (Equality tv1 tv2) (Equality function1 function2) -> pure (tv1, tv2)
                _ -> expectationFailure $ "Expected equality constraint, got: " ++ showColored constraint
            case ty of
                Scalar ScalarInt | tv1 == tv2 -> pass
                _ -> expectationFailure $ "Expected int type, got: " ++ showColored ty

prop_literalTypesInvariants :: Property
prop_literalTypesInvariants = property $ do
    literalGen <-
        forAll $
            Gen.choice
                [ mkIntExpr <$> Gen.int (Range.linear minBound maxBound)
                , mkFloatExpr <$> Gen.double (Range.linearFrac (-1000) 1000)
                , mkStringExpr <$> Gen.text (Range.linear 0 100) Gen.alphaNum
                ]

    (constraint, exprType) <- evalEitherM $ evalIO $ runInfer $ generateConstraints emptyTypeEnvironment literalGen

    case exprType of
        Scalar _ -> pass
        _ -> failure

runInfer :: Sem (InferEffects loc) a -> IO (Either (InferError loc) (Constraint loc, a))
runInfer =
    fmap (\x -> fmap (\(cons, y) -> (tidyConstraint cons, y)) x)
        . runM @IO
        . uniqueGenToIO
        . runError
        . runWriter
        . subsume_

shouldSucceed ::
    (HasCallStack, Show (InferError loc)) =>
    Either (InferError loc) (Constraint loc, a) ->
    ((Constraint loc, a) -> Expectation) ->
    Expectation
shouldSucceed (Left err) _ = withFrozenCallStack $ expectationFailure $ "Inference failed: " ++ show err
shouldSucceed (Right result) assertion = assertion result

-- Utility Matchers
isScalarInt :: Monotype loc -> Bool
isScalarInt (Scalar ScalarInt) = True
isScalarInt _ = False

isScalarFloat :: Monotype loc -> Bool
isScalarFloat (Scalar ScalarFloat) = True
isScalarFloat _ = False

mkIntExpr :: Int -> ShuntedExpr
mkIntExpr i = mkExpr (Int $ fromIntegral i)

mkFloatExpr :: Double -> ShuntedExpr
mkFloatExpr f = mkExpr (Float f)

mkStringExpr :: Text -> ShuntedExpr
mkStringExpr s = mkExpr (String s)

mkExpr :: ShuntedExpr' -> ShuntedExpr
mkExpr expr = Expr (testLocated expr, Nothing)
