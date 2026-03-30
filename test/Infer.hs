{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Type inference tests
module Infer (spec) where

import Boilerplate (fakeTypeEnvironment, runQueryEffects)
import Common (evalReportableM)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (evalState)
import Effectful.Writer.Static.Local (runWriter)
import Elara.AST.Name (Qualified, TypeName)
import Elara.AST.Phases.Shunted (ShuntedExpr, ShuntedExpr')
import Elara.AST.Region (SourceRegion)
import Elara.AST.Types qualified as New
import Elara.Data.Pretty (AnsiStyle)
import Elara.Error (ReportableError (..), SomeReportableError, runErrorOrReport)
import Elara.Error.Effect (evalDiagnosticWriter)
import Elara.Prim (floatName, intName, mkPrimQual, stringName)
import Elara.TypeInfer.ConstraintGeneration (generateConstraints)
import Elara.TypeInfer.Context (emptyContextStack)
import Elara.TypeInfer.Environment (InferError, emptyLocalTypeEnvironment)
import Elara.TypeInfer.Error (UnifyError)
import Elara.TypeInfer.Type (Constraint, Monotype (..))
import Hedgehog (Property, annotateShow, evalIO, failure, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Infer.Unify qualified as Unify
import Print (prettyToString)
import Region (testRegion)
import Test.Syd
import Test.Syd.Hedgehog ()
import Prelude hiding (fail)

spec :: Spec
spec = describe "Infers types correctly" $ do
    literalTests
    it "infers literals" prop_literalTypesInvariants
    additionalLiteralTests
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
        . runReader emptyContextStack

-- | Helper to check if a type is a specific primitive type constructor
isPrimType :: Qualified TypeName -> Monotype loc -> Bool
isPrimType expected (TypeConstructor _ actual []) = expected == actual
isPrimType _ _ = False

literalTests :: Spec
literalTests = describe "Literal Type Inference" $ do
    it "infers Int type correctly" $ do
        result <- runInfer @SourceRegion $ generateConstraints (mkIntExpr 42)
        result `shouldSucceed` \((_, ty), constraints) -> do
            constraints `shouldBe` mempty
            isPrimType (mkPrimQual intName) ty `shouldBe` True

    it "infers Float type correctly" $ do
        result <- runInfer $ generateConstraints (mkFloatExpr 42.0)
        result `shouldSucceed` \((_, ty), constraints) -> do
            constraints `shouldBe` mempty
            isPrimType (mkPrimQual floatName) ty `shouldBe` True

    it "infers String type correctly" $ do
        result <- runInfer $ generateConstraints (mkStringExpr "hello")
        result `shouldSucceed` \((_, ty), constraints) -> do
            constraints `shouldBe` mempty
            isPrimType (mkPrimQual stringName) ty `shouldBe` True

{- | Property test to ensure that literal types maintain invariants.
Specifically, that the inferred type for literals is always a primitive type constructor.
-}
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

    -- Check that the type is a TypeConstructor
    case ty of
        TypeConstructor{} -> pass
        other -> annotateShow other *> failure

-- | Additional literal type inference tests
additionalLiteralTests :: Spec
additionalLiteralTests = describe "Additional Literal Type Inference" $ do
    it "infers Char literal correctly" $ do
        let charExpr = mkExpr New.EChar 'a'
        result <- runInfer @SourceRegion $ generateConstraints charExpr
        result `shouldSucceed` \((_, ty), _) -> do
            case ty of
                TypeConstructor{} -> pass
                other -> expectationFailure $ "Expected TypeConstructor for Char, got: " ++ show other

    it "infers Unit literal correctly" $ do
        let unitExpr = mkExpr' New.EUnit
        result <- runInfer @SourceRegion $ generateConstraints unitExpr
        result `shouldSucceed` \((_, ty), _) -> do
            case ty of
                TypeConstructor{} -> pass
                other -> expectationFailure $ "Expected TypeConstructor for Unit, got: " ++ show other

    it "infers negative Int literal correctly" $ do
        result <- runInfer @SourceRegion $ generateConstraints (mkIntExpr (-42))
        result `shouldSucceed` \((_, ty), _) -> do
            isPrimType (mkPrimQual intName) ty `shouldBe` True

    it "infers empty String correctly" $ do
        result <- runInfer @SourceRegion $ generateConstraints (mkStringExpr "")
        result `shouldSucceed` \((_, ty), _) -> do
            isPrimType (mkPrimQual stringName) ty `shouldBe` True

-- | Helper to assert that inference succeeded
shouldSucceed :: Either SomeReportableError b -> (b -> IO a3) -> IO a3
shouldSucceed (Left err) _ =
    withFrozenCallStack $ expectationFailure $ "Inference failed: " ++ prettyToString ((\x -> x @AnsiStyle) <$> errorCode err)
shouldSucceed (Right result) assertion = withFrozenCallStack $ assertion result

-- | Create a 'ShuntedExpr' representing an integer literal
mkIntExpr :: Int -> ShuntedExpr
mkIntExpr i = mkExpr' (New.EInt $ fromIntegral i)

-- | Create a 'ShuntedExpr' representing a float literal
mkFloatExpr :: Double -> ShuntedExpr
mkFloatExpr f = mkExpr' (New.EFloat f)

-- | Create a 'ShuntedExpr' representing a string literal
mkStringExpr :: Text -> ShuntedExpr
mkStringExpr s = mkExpr' (New.EString s)

-- | Helper to create a 'ShuntedExpr' from a 'ShuntedExpr''
mkExpr' :: ShuntedExpr' -> ShuntedExpr
mkExpr' = New.Expr testRegion Nothing

-- | Helper to create a 'ShuntedExpr' from a constructor taking one argument
mkExpr :: (a -> ShuntedExpr') -> a -> ShuntedExpr
mkExpr f x = mkExpr' (f x)
