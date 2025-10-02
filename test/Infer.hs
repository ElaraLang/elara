{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-- Literal Type Inference Tests
-- devious and evil
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Infer where

import Boilerplate (ensureExpressionMatches, evalPipelineRes, fakeTypeEnvironment, loadShuntedExpr, runQueryEffects, shouldMatch)
import Common (evalReportableM)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.State.Static.Local (evalState, put)
import Effectful.Writer.Static.Local (runWriter)
import Elara.AST.Generic.Types (Expr (..), Expr' (..))
import Elara.AST.Region (SourceRegion)
import Elara.AST.Shunted
import Elara.Data.Pretty
import Elara.Error (ReportableError (..), SomeReportableError, runErrorOrReport)
import Elara.Error.Effect (evalDiagnosticWriter)
import Elara.TypeInfer (inferValue)
import Elara.TypeInfer.ConstraintGeneration
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Type
import Hedgehog (MonadTest, Property, annotate, evalIO, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Range qualified as Range
import Infer.Unify qualified as Unify
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

runInfer :: forall loc a. loc ~ SourceRegion => _ -> IO (Either SomeReportableError (a, Constraint loc))
runInfer =
    runEff
        . runQueryEffects
        . evalState emptyLocalTypeEnvironment
        . evalState fakeTypeEnvironment
        . evalDiagnosticWriter
        . runErrorNoCallStack @SomeReportableError
        . runErrorOrReport @(InferError loc)
        . runErrorOrReport @(UnifyError loc)
        . runWriter @(Constraint loc)

literalTests :: Spec
literalTests = describe "Literal Type Inference" $ do
    it "infers Int type correctly" $ do
        result <- runInfer @SourceRegion $ generateConstraints (mkIntExpr 42)
        result `shouldSucceed` \((_, ty), constraints) -> do
            constraints `shouldBe` mempty
            $(shouldMatch [p|(Scalar _ ScalarInt)|]) ty

    it "infers Float type correctly" $ do
        result <- runInfer $ generateConstraints (mkFloatExpr 42.0)
        result `shouldSucceed` \((_, ty), constraints) -> do
            constraints `shouldBe` mempty
            $(shouldMatch [p|(Scalar _ ScalarFloat)|]) ty

lambdaTests :: Spec
lambdaTests = withoutRetries $ describe "Lambda Type Inference" $ do
    it "infers lambda type correctly" $ property $ do
        expr <- inferFully "\\x -> x"

        case expr of
            Forall _ [a] (EmptyConstraint _) (Function _ (TypeVar _ (UnificationVar b)) (TypeVar _ (UnificationVar c))) | a == b && b == c -> pass
            other -> failWith Nothing $ "Expected function type, got: " <> toString (showPretty other)

    it "infers applied identity function correctly" $ property $ do
        expr <- inferFully "(\\x -> x) 42"

        $(ensureExpressionMatches [p|Forall _ [] _ (Scalar _ ScalarInt)|]) expr

    it "infers nested identity function correctly" $ property $ do
        expr <- inferFully "(\\x -> (\\y -> y) x) 42"

        $(ensureExpressionMatches [p|Forall _ [] _ (Scalar _ ScalarInt)|]) expr

    it "infers id id correctly" $ property $ do
        expr <- inferFully "let id = \\x -> x in id id"

        case expr of
            Forall _ [a] EmptyConstraint{} (Function _ (TypeVar _ (UnificationVar b)) (TypeVar _ (UnificationVar c))) | a == b && b == c -> pass
            other -> failWith Nothing $ "Expected function type, got: " <> toString (showPretty other)

letInTests :: Spec
letInTests = withoutRetries $ describe "Let In Type Inference" $ do
    it "infers let in type correctly" $ property $ do
        expr <- inferFully "let x = 42 in x"

        $(ensureExpressionMatches [p|Forall _ [] (EmptyConstraint{}) (Scalar _ ScalarInt)|]) expr

    it "infers let in type correctly with shadowing" $ property $ do
        expr <- inferFully "let x = 42 in let x = 43 in x"

        $(ensureExpressionMatches [p|Forall _ [] (EmptyConstraint{}) (Scalar _ ScalarInt)|]) expr

    it "infers let in type correctly with shadowing and lambda" $ property $ do
        expr <- inferFully "let x = 42 in let f = \\x -> x in f x"

        $(ensureExpressionMatches [p|Forall _ [] (EmptyConstraint{}) (Scalar _ ScalarInt)|]) expr

recursionTests :: Spec
recursionTests = withoutRetries $ describe "recursion tests" $ do
    it "recursion" $ property $ do
        expr <- inferFully "let loop x = if x == 0 then x else loop (x - 1) in loop"

        $(ensureExpressionMatches [p|Forall _ [] _ (Function _ (Scalar _ ScalarInt) (Scalar _ ScalarInt))|]) expr

ifElseTests :: Spec
ifElseTests = describe "If Else Type Inference" $ do
    it "infers if else type correctly" $ property $ do
        expr <- inferFully "\\x -> if x then 42 else 43"

        $(ensureExpressionMatches [p|Forall _ [] _ (Function _ _ (Scalar _ ScalarInt))|]) expr

inferFully :: (MonadTest m, MonadIO m) => Text -> m (Polytype SourceRegion)
inferFully exprSrc = do
    let expr = loadShuntedExpr exprSrc
    Hedgehog.annotate $ toString exprSrc
    res <- evalPipelineRes expr
    r <- liftIO $ runInfer $ do
        put fakeTypeEnvironment
        inferValue (qualifiedTest "test") res Nothing
    case r of
        Left err -> failWith Nothing $ "Inference failed: " ++ prettyToString ((\x -> x) <$> errorCode err :: Maybe (Doc AnsiStyle))
        Right (ty, _) -> pure (snd ty)

prop_literalTypesInvariants :: Property
prop_literalTypesInvariants = property $ do
    literalGen <-
        forAll $
            Gen.choice
                [ mkIntExpr <$> Gen.int (Range.linear minBound maxBound)
                , mkFloatExpr <$> Gen.double (Range.linearFrac (-1000) 1000)
                , mkStringExpr <$> Gen.text (Range.linear 0 100) Gen.alphaNum
                ]

    ((_, ty), _) <- evalReportableM $ evalIO $ runInfer $ generateConstraints literalGen

    $(ensureExpressionMatches [p|Scalar _ _|]) ty

shouldSucceed :: Either SomeReportableError b -> (b -> IO a3) -> IO a3
shouldSucceed (Left err) _ =
    {-# HLINT ignore "Use id" #-}
    -- i love impredicative types
    withFrozenCallStack $ expectationFailure $ "Inference failed: " ++ prettyToString ((\x -> x) <$> errorCode err :: Maybe (Doc AnsiStyle))
shouldSucceed (Right result) assertion = withFrozenCallStack $ assertion result

mkIntExpr :: Int -> ShuntedExpr
mkIntExpr i = mkExpr (Int $ fromIntegral i)

mkFloatExpr :: Double -> ShuntedExpr
mkFloatExpr f = mkExpr (Float f)

mkStringExpr :: Text -> ShuntedExpr
mkStringExpr s = mkExpr (String s)

mkExpr :: ShuntedExpr' -> ShuntedExpr
mkExpr expr = Expr (testLocated expr, Nothing)
