{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Core to ANF conversion
module ToANF (spec) where

import Effectful
import Elara.AST.Name ()
import Elara.AST.VarRef (VarRef' (Local))
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Generic (Bind (..))
import Elara.Core.Pretty ()
import Elara.Core.ToANF (fromANF, toANF)
import Elara.Data.Unique (Unique (..))
import Elara.Data.Unique.Effect (uniqueGenToGlobalIO)
import Elara.Logging (ignoreStructuredDebug)
import Elara.Prim.Core (intCon)
import Test.Syd

intType :: Core.Type
intType = Core.ConTy intCon

mkVar :: Text -> Core.Type -> Core.Var
mkVar name ty = Core.Id (Local (Unique name 0)) ty Nothing

-- | Run ANF conversion
runToANF :: Core.CoreExpr -> IO (ANF.Expr Core.Var)
runToANF expr =
    runEff $
        uniqueGenToGlobalIO $
            ignoreStructuredDebug $
                toANF expr

spec :: Spec
spec = describe "Core to ANF conversion" $ do
    atomicTests
    applicationTests
    letTests
    roundTripTests

atomicTests :: Spec
atomicTests = describe "Atomic expressions stay atomic" $ do
    it "literals are atomic" $ do
        result <- runToANF (Core.Lit (Core.Int 42))
        result `shouldBe` ANF.CExpr (ANF.AExpr (ANF.Lit (Core.Int 42)))

    it "variables are atomic" $ do
        let x = mkVar "x" intType
        result <- runToANF (Core.Var x)
        result `shouldBe` ANF.CExpr (ANF.AExpr (ANF.Var x))

    it "lambdas are atomic" $ do
        let x = mkVar "x" intType
        result <- runToANF (Core.Lam x (Core.Var x))
        case result of
            ANF.CExpr (ANF.AExpr (ANF.Lam _ _)) -> pass
            other -> expectationFailure $ "Expected atomic lambda, got: " ++ show other

applicationTests :: Spec
applicationTests = describe "Applications are normalized" $ do
    it "simple application stays flat" $ do
        let f = mkVar "f" (Core.FuncTy intType intType)
        let x = mkVar "x" intType
        result <- runToANF (Core.App (Core.Var f) (Core.Var x))
        -- f x is already in ANF: both are atomic
        result `shouldBe` ANF.CExpr (ANF.App (ANF.Var f) (ANF.Var x))

    it "nested application introduces let binding" $ do
        -- f (g x) should become: let v = g x in f v
        let f = mkVar "f" (Core.FuncTy intType intType)
        let g = mkVar "g" (Core.FuncTy intType intType)
        let x = mkVar "x" intType
        result <- runToANF (Core.App (Core.Var f) (Core.App (Core.Var g) (Core.Var x)))
        case result of
            ANF.Let (NonRecursive (_, ANF.App (ANF.Var g') (ANF.Var x'))) (ANF.CExpr (ANF.App (ANF.Var f') (ANF.Var _))) -> do
                g' `shouldBe` g
                x' `shouldBe` x
                f' `shouldBe` f
            other -> expectationFailure $ "Expected let-bound application, got: " ++ show other

    it "deeply nested application linearizes" $ do
        -- f (g (h x)) should introduce 2 let bindings
        let f = mkVar "f" (Core.FuncTy intType intType)
        let g = mkVar "g" (Core.FuncTy intType intType)
        let h = mkVar "h" (Core.FuncTy intType intType)
        let x = mkVar "x" intType
        let expr = Core.App (Core.Var f) (Core.App (Core.Var g) (Core.App (Core.Var h) (Core.Var x)))
        result <- runToANF expr
        -- Count let bindings
        let countLets (ANF.Let _ body) = 1 + countLets body
            countLets _ = 0 :: Int
        countLets result `shouldBe` 2

letTests :: Spec
letTests = describe "Let bindings" $ do
    it "preserves simple let binding" $ do
        -- let x = 5 in x
        let x = mkVar "x" intType
        result <- runToANF (Core.Let (NonRecursive (x, Core.Lit (Core.Int 5))) (Core.Var x))
        case result of
            ANF.Let (NonRecursive (x', ANF.AExpr (ANF.Lit (Core.Int 5)))) (ANF.CExpr (ANF.AExpr (ANF.Var x''))) -> do
                x' `shouldBe` x
                x'' `shouldBe` x
            other -> expectationFailure $ "Expected let binding, got: " ++ show other

    it "preserves recursive let binding" $ do
        -- let rec f = \x -> f x in f
        let x = mkVar "x" intType
        let f = mkVar "f" (Core.FuncTy intType intType)
        let expr = Core.Let (Recursive [(f, Core.Lam x (Core.App (Core.Var f) (Core.Var x)))]) (Core.Var f)
        result <- runToANF expr
        case result of
            ANF.Let (Recursive _) _ -> pass
            other -> expectationFailure $ "Expected recursive let, got: " ++ show other

roundTripTests :: Spec
roundTripTests = describe "ANF round-trip (fromANF . toANF)" $ do
    it "round-trips a literal" $ do
        let expr = Core.Lit (Core.Int 42)
        result <- runToANF expr
        fromANF result `shouldBe` expr

    it "round-trips a variable" $ do
        let x = mkVar "x" intType
        let expr = Core.Var x
        result <- runToANF expr
        fromANF result `shouldBe` expr

    it "round-trips a simple application" $ do
        let f = mkVar "f" (Core.FuncTy intType intType)
        let x = mkVar "x" intType
        let expr = Core.App (Core.Var f) (Core.Var x)
        result <- runToANF expr
        fromANF result `shouldBe` expr

    it "round-trips a lambda" $ do
        let x = mkVar "x" intType
        let expr = Core.Lam x (Core.Var x)
        result <- runToANF expr
        fromANF result `shouldBe` expr
