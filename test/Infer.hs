module Infer where

import Boilerplate (loadShuntedExpr)
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
import Optics.Operators.Unsafe ((^?!))
import Polysemy (Sem, run, runM, subsume, subsume_)
import Polysemy.Error (runError)
import Polysemy.Writer (runWriter)
import Print (showPretty)
import Region (qualifiedTest, testLocated)
import Relude.Unsafe ((!!))
import Test.Hspec
import Test.Hspec.Hedgehog hiding (Var)
import Prelude hiding (fail)

spec :: Spec
spec = describe "Infers types correctly" $ parallel $ do
    literalTests

-- variableTests

-- Literal Type Inference Tests
literalTests :: Spec
literalTests = describe "Literal Type Inference" $ do
    it "infers Int type correctly" $ do
        result <- runInfer $ generateConstraints emptyTypeEnvironment (mkIntExpr 42)
        result `shouldSucceed` \(constraints, ty) -> do
            constraints `shouldBe` []
            ty `shouldSatisfy` isScalarInt

    it "infers Float type correctly" $ do
        result <- runInfer $ generateConstraints emptyTypeEnvironment (mkFloatExpr 42.0)
        result `shouldSucceed` \(constraints, ty) -> do
            constraints `shouldBe` []
            ty `shouldSatisfy` isScalarFloat

-- lambdaTests :: Spec
-- lambdaTests = describe "Lambda Type Inference" $ do
--     it "infers lambda type correctly" $ do
--         expr <- loadShuntedExpr "\\x -> x"
--         result <- runInfer $ generateConstraints emptyTypeEnvironment expr
--         result `shouldSucceed` \(constraints, ty) -> do
--             constraints `shouldBe` []
--             ty `shouldBe` Function (TypeVar 0) (Scalar ScalarInt)

runInfer :: Sem (InferEffects loc) a -> IO (Either (InferError loc) ([Constraint loc], a))
runInfer =
    runM @IO
        . uniqueGenToIO
        . runError
        . runWriter
        . subsume_

shouldSucceed ::
    (HasCallStack, Show (InferError loc)) =>
    Either (InferError loc) ([Constraint loc], a) ->
    (([Constraint loc], a) -> Expectation) ->
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

mkExpr :: ShuntedExpr' -> ShuntedExpr
mkExpr expr = Expr (testLocated expr, Nothing)
