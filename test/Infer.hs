{-# LANGUAGE TemplateHaskell #-}

module Infer where

import Boilerplate (ensureExpressionMatches, loadShuntedExpr, pipelineResShouldSucceed, shouldMatch)
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
import GHC.Stack (withFrozenCallStack)
import Hedgehog (Property, annotate, assert, evalEither, evalEitherM, evalIO, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Range qualified as Range
import Infer.Unify qualified as Unify
import Optics.Operators.Unsafe ((^?!))
import Polysemy (Sem, run, runM, subsume, subsume_)
import Polysemy.Error (runError)
import Polysemy.State (evalState, runState)
import Polysemy.Writer (runWriter)
import Print (prettyToString, printColored, printPretty, showColored, showPretty)
import Region (qualifiedTest, testLocated)
import Relude.Unsafe ((!!))
import Test.Syd
import Test.Syd.Expectation
import Test.Syd.Hedgehog ()
import Prelude hiding (fail)

spec :: Spec
spec = describe "Infers types correctly" $ do
    literalTests
    lambdaTests
    letInTests
    it "infers literals" prop_literalTypesInvariants
    Unify.spec

-- Literal Type Inference Tests
literalTests :: Spec
literalTests = describe "Literal Type Inference" $ do
    it "infers Int type correctly" $ do
        result <- runInfer $ generateConstraints emptyTypeEnvironment (mkIntExpr 42)
        result `shouldSucceed` \(constraints, (intExp, ty)) -> do
            constraints `shouldBe` mempty
            $(shouldMatch [p|(Scalar ScalarInt)|]) ty

    it "infers Float type correctly" $ do
        result <- runInfer $ generateConstraints emptyTypeEnvironment (mkFloatExpr 42.0)
        result `shouldSucceed` \(constraints, (exp, ty)) -> do
            constraints `shouldBe` mempty
            $(shouldMatch [p|(Scalar ScalarFloat)|]) ty

lambdaTests :: Spec
lambdaTests = describe "Lambda Type Inference" $ do
    it "infers lambda type correctly" $ do
        let expr = loadShuntedExpr "\\x -> x"
        res <- pipelineResShouldSucceed expr
        result <- liftIO $ runInfer $ generateConstraints emptyTypeEnvironment res

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

    withoutRetries $ it "infers nested identity function correctly" $ property $ do
        expr <- inferFully "(\\x -> (\\y -> y) x) 42"

        expr === Scalar ScalarInt

letInTests :: Spec
letInTests = describe "Let In Type Inference" $ do
    it "infers let in type correctly" $ property $ do
        expr <- inferFully "let x = 42 in x"

        expr === Scalar ScalarInt

    it "infers let in type correctly with shadowing" $ property $ do
        expr <- inferFully "let x = 42 in let x = 43 in x"

        expr === Scalar ScalarInt

    it "infers let in type correctly with shadowing and lambda" $ property $ do
        expr <- inferFully "let x = 42 in let f = \\x -> x in f x"

        expr === Scalar ScalarInt

inferFully exprSrc = do
    let expr = loadShuntedExpr exprSrc
    res <- liftIO $ pipelineResShouldSucceed expr
    (constraint, (exp, ty)) <- evalEitherM $ liftIO $ runInfer $ generateConstraints emptyTypeEnvironment res
    annotate $ prettyToString constraint
    (subst, newConstraint) <- evalEither $ Unify.runUnify $ unifyEquality constraint
    annotate $ prettyToString newConstraint
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

    (_, (_, ty)) <- evalEitherM $ evalIO $ runInfer $ generateConstraints emptyTypeEnvironment literalGen

    $(ensureExpressionMatches [p|Scalar _|]) ty

runInfer :: Sem (InferEffects loc) a -> IO (Either (InferError loc) (Constraint loc, a))
runInfer =
    runM @IO
        . uniqueGenToIO
        . runError
        . evalState emptyLocalTypeEnvironment
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
