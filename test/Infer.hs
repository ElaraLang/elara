{-# LANGUAGE TemplateHaskell #-}

module Infer where

import Boilerplate (ensureExpressionMatches, evalPipelineRes, fakeTypeEnvironment, loadShuntedExpr, pipelineResShouldSucceed, shouldMatch)
import Elara.AST.Generic.Types (Expr (..), Expr' (..))
import Elara.AST.Shunted
import Elara.Data.Unique (uniqueGenToIO)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Type
import Hedgehog (Property, annotate, evalEither, evalEitherM, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Infer.Unify qualified as Unify
import Polysemy (Sem, runM, subsume_)
import Polysemy.Error (runError)
import Polysemy.State (evalState)
import Polysemy.Writer (runWriter)
import Print (prettyToString)
import Region (testLocated)
import Test.Syd
import Test.Syd.Hedgehog ()
import Prelude hiding (fail)

spec :: Spec
spec = describe "Infers types correctly" $ do
    literalTests
    lambdaTests
    letInTests
    ifElseTests
    recursionTests
    it "infers literals" prop_literalTypesInvariants
    Unify.spec

-- Literal Type Inference Tests
literalTests :: Spec
literalTests = describe "Literal Type Inference" $ do
    it "infers Int type correctly" $ do
        result <- runInfer $ generateConstraints (mkIntExpr 42)
        result `shouldSucceed` \(constraints, (intExp, ty)) -> do
            constraints `shouldBe` mempty
            $(shouldMatch [p|(Scalar ScalarInt)|]) ty

    it "infers Float type correctly" $ do
        result <- runInfer $ generateConstraints (mkFloatExpr 42.0)
        result `shouldSucceed` \(constraints, (exp, ty)) -> do
            constraints `shouldBe` mempty
            $(shouldMatch [p|(Scalar ScalarFloat)|]) ty

lambdaTests :: Spec
lambdaTests = withoutRetries $ describe "Lambda Type Inference" $ do
    it "infers lambda type correctly" $ do
        let expr = loadShuntedExpr "\\x -> x"
        res <- pipelineResShouldSucceed expr
        result <- liftIO $ runInfer $ generateConstraints res

        result `shouldSucceed` \(constraint, (exp, ty)) -> do
            case constraint of
                EmptyConstraint -> pure ()
                _ -> expectationFailure $ "Expected empty constraint, got: " ++ show constraint
            case ty of
                Function a b | a == b -> pass
                _ -> expectationFailure $ "Expected function type, got: " ++ show ty

    it "infers applied identity function correctly" $ property $ do
        expr <- inferFully "(\\x -> x) 42"

        expr === Scalar ScalarInt

    it "infers nested identity function correctly" $ property $ do
        expr <- inferFully "(\\x -> (\\y -> y) x) 42"

        expr === Scalar ScalarInt

letInTests :: Spec
letInTests = withoutRetries $ describe "Let In Type Inference" $ do
    it "infers let in type correctly" $ property $ do
        expr <- inferFully "let x = 42 in x"

        expr === Scalar ScalarInt

    it "infers let in type correctly with shadowing" $ property $ do
        expr <- inferFully "let x = 42 in let x = 43 in x"

        expr === Scalar ScalarInt

    it "infers let in type correctly with shadowing and lambda" $ property $ do
        expr <- inferFully "let x = 42 in let f = \\x -> x in f x"

        expr === Scalar ScalarInt

recursionTests :: Spec
recursionTests = withoutRetries $ describe "recursion tests" $ do
    it "recursion" $ property $ do
        expr <- inferFully "let loop x = if x == 0 then x else loop (x - 1) in loop"

        expr === Function (Scalar ScalarInt) (Scalar ScalarInt)

ifElseTests :: Spec
ifElseTests = describe "If Else Type Inference" $ do
    it "infers if else type correctly" $ property $ do
        expr <- inferFully "if True then 42 else 43"

        expr === Scalar ScalarInt

inferFully exprSrc = do
    let expr = loadShuntedExpr exprSrc
    res <- evalPipelineRes expr
    (constraint, (exp, ty)) <- evalEitherM $ liftIO $ runInfer $ generateConstraints res
    annotate $ prettyToString constraint
    (subst, newConstraint) <- evalEither $ Unify.runUnify $ unifyEquality constraint
    annotate $ prettyToString newConstraint
    newConstraint === EmptyConstraint -- there should be no residual
    annotate $ prettyToString subst
    pure (substituteAll subst ty)

prop_literalTypesInvariants :: Property
prop_literalTypesInvariants = property $ do
    literalGen <-
        forAll $
            Gen.choice
                [ mkIntExpr <$> Gen.int (Range.linear minBound maxBound)
                , mkFloatExpr <$> Gen.double (Range.linearFrac (-1000) 1000)
                , mkStringExpr <$> Gen.text (Range.linear 0 100) Gen.alphaNum
                ]

    (_, (_, ty)) <- evalEitherM $ evalIO $ runInfer $ generateConstraints literalGen

    $(ensureExpressionMatches [p|Scalar _|]) ty

runInfer :: Sem (InferEffects loc) a -> IO (Either (InferError loc) (Constraint loc, a))
runInfer =
    runM @IO
        . uniqueGenToIO
        . runError
        . evalState emptyLocalTypeEnvironment
        . evalState fakeTypeEnvironment
        . runWriter
        . subsume_

shouldSucceed ::
    (HasCallStack, Show (InferError loc)) =>
    Either (InferError loc) (Constraint loc, a) ->
    ((Constraint loc, a) -> IO b) ->
    IO b
shouldSucceed (Left err) _ = withFrozenCallStack $ expectationFailure $ "Inference failed: " ++ show err
shouldSucceed (Right result) assertion = withFrozenCallStack $ assertion result

mkIntExpr :: Int -> ShuntedExpr
mkIntExpr i = mkExpr (Int $ fromIntegral i)

mkFloatExpr :: Double -> ShuntedExpr
mkFloatExpr f = mkExpr (Float f)

mkStringExpr :: Text -> ShuntedExpr
mkStringExpr s = mkExpr (String s)

mkExpr :: ShuntedExpr' -> ShuntedExpr
mkExpr expr = Expr (testLocated expr, Nothing)
