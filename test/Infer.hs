{-# LANGUAGE TemplateHaskell #-}

module Infer where

import Boilerplate (ensureExpressionMatches, evalPipelineRes, fakeTypeEnvironment, loadShuntedExpr, pipelineResShouldSucceed, shouldMatch)
import Elara.AST.Generic.Types (Expr (..), Expr' (..))
import Elara.AST.Shunted
import Elara.Data.Unique (uniqueGenToIO)
import Elara.Logging (ignoreStructuredDebug, structuredDebugToLog)
import Elara.Pipeline (finalisePipeline)
import Elara.TypeInfer (inferValue, runInferPipeline)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Monad
import Elara.TypeInfer.Type
import Hedgehog (Property, annotate, evalEither, evalEitherM, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Range qualified as Range
import Infer.Unify qualified as Unify
import Polysemy (Sem, runM, subsume_)
import Polysemy.Error (runError)
import Polysemy.State (evalState, put)
import Polysemy.Writer (runWriter)
import Print (prettyToString, showPretty)
import Region (qualifiedTest, testLocated)
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
    it "infers lambda type correctly" $ property $ do
        expr <- inferFully "\\x -> x"

        case expr of
            Forall [a] EmptyConstraint (Function (TypeVar (UnificationVar b)) (TypeVar (UnificationVar c))) | a == b && b == c -> pure ()
            other -> failWith Nothing $ "Expected function type, got: " <> toString (showPretty other)

    it "infers applied identity function correctly" $ property $ do
        expr <- inferFully "(\\x -> x) 42"

        expr === Forall [] EmptyConstraint (Scalar ScalarInt)

    it "infers nested identity function correctly" $ property $ do
        expr <- inferFully "(\\x -> (\\y -> y) x) 42"

        expr === Forall [] EmptyConstraint (Scalar ScalarInt)

    it "infers id id correctly" $ property $ do
        expr <- inferFully "let id = \\x -> x in id id"

        case expr of
            Forall [a] EmptyConstraint (Function (TypeVar (UnificationVar b)) (TypeVar (UnificationVar c))) | a == b && b == c -> pure ()
            other -> failWith Nothing $ "Expected function type, got: " <> toString (showPretty other)

letInTests :: Spec
letInTests = withoutRetries $ describe "Let In Type Inference" $ do
    it "infers let in type correctly" $ property $ do
        expr <- inferFully "let x = 42 in x"

        expr === Forall [] EmptyConstraint (Scalar ScalarInt)

    it "infers let in type correctly with shadowing" $ property $ do
        expr <- inferFully "let x = 42 in let x = 43 in x"

        expr === Forall [] EmptyConstraint (Scalar ScalarInt)

    it "infers let in type correctly with shadowing and lambda" $ property $ do
        expr <- inferFully "let x = 42 in let f = \\x -> x in f x"

        expr === Forall [] EmptyConstraint (Scalar ScalarInt)

recursionTests :: Spec
recursionTests = withoutRetries $ describe "recursion tests" $ do
    it "recursion" $ property $ do
        expr <- inferFully "let loop x = if x == 0 then x else loop (x - 1) in loop"

        expr === Forall [] EmptyConstraint (Function (Scalar ScalarInt) (Scalar ScalarInt))

ifElseTests :: Spec
ifElseTests = describe "If Else Type Inference" $ do
    it "infers if else type correctly" $ property $ do
        expr <- inferFully "\\x -> if x then 42 else 43"

        $(ensureExpressionMatches [p|Forall [] _ (Function _ (Scalar ScalarInt)) |]) expr
        

inferFully exprSrc = do
    let expr = loadShuntedExpr exprSrc
    annotate $ toString exprSrc
    res <- evalPipelineRes expr
    (_, ty) <- evalPipelineRes $ finalisePipeline $ runInferPipeline $ ignoreStructuredDebug $ do
        put fakeTypeEnvironment -- devious and evil
        inferValue (qualifiedTest "test") res Nothing
    pure ty

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
        . ignoreStructuredDebug
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
