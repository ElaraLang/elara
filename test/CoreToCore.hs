{-# LANGUAGE ImportQualifiedPost #-}

-- | Tests for Core-to-Core optimisation passes
module CoreToCore (spec) where

import Elara.AST.Name (ModuleName (..), Qualified (..))
import Elara.AST.VarRef (VarRef' (..))
import Elara.Core (CoreExpr, Expr (..), Literal (..), Var (..))
import Elara.Core qualified as Core
import Elara.Core.Generic (Bind (..))
import Elara.CoreToCore (betaReduce, constantFold, coreToCoreExpr, fullCoreToCoreExpr, pipeInline, subst, uselessLetInline)
import Elara.Data.Unique (Unique (..))
import Elara.Prim.Core (intCon)
import Test.Syd

-- | Create a local variable
mkVar :: Text -> Core.Type -> Var
mkVar name ty = Id (Local (Unique name 0)) ty Nothing

-- | Create a global variable in a given module
mkGlobalVar :: NonEmpty Text -> Text -> Core.Type -> Var
mkGlobalVar modParts name ty = Id (Global (Qualified name (ModuleName modParts))) ty Nothing

intType :: Core.Type
intType = Core.ConTy intCon

-- | Create a Prelude operator reference (for + etc.)
preludeOp :: Text -> CoreExpr
preludeOp name = Var (mkGlobalVar ("Elara" :| ["Prim"]) name intType)

-- | Create an Elara.Prim operator reference (for |> etc.)
primOp :: Text -> CoreExpr
primOp name = Var (mkGlobalVar ("Prelude" :| []) name intType)

spec :: Spec
spec = describe "Core-to-Core optimizations" $ do
    constantFoldTests
    betaReduceTests
    uselessLetInlineTests
    pipeInlineTests
    substTests
    composedPassTests

constantFoldTests :: Spec
constantFoldTests = describe "Constant folding" $ do
    it "folds integer addition" $ do
        -- 1 + 2 => 3
        let expr = App (App (preludeOp "+") (Lit (Int 1))) (Lit (Int 2))
        constantFold expr `shouldBe` Lit (Int 3)

    it "folds addition with zero" $ do
        -- 0 + 5 => 5
        let expr = App (App (preludeOp "+") (Lit (Int 0))) (Lit (Int 5))
        constantFold expr `shouldBe` Lit (Int 5)

    it "folds negative integers" $ do
        -- (-3) + 7 => 4
        let expr = App (App (preludeOp "+") (Lit (Int (-3)))) (Lit (Int 7))
        constantFold expr `shouldBe` Lit (Int 4)

    it "does not fold non-literal operands" $ do
        -- x + 2 should remain unchanged
        let x = mkVar "x" intType
        let expr = App (App (preludeOp "+") (Var x)) (Lit (Int 2))
        constantFold expr `shouldBe` expr

    it "folds nested additions" $ do
        -- (1 + 2) + 3 => 6
        let inner = App (App (preludeOp "+") (Lit (Int 1))) (Lit (Int 2))
        let expr = App (App (preludeOp "+") inner) (Lit (Int 3))
        -- After one pass, inner folds to 3, then outer folds to 6
        constantFold expr `shouldBe` Lit (Int 6)

    it "does not fold non-Prelude +" $ do
        -- Custom.+ 1 2 should not fold
        let customPlus = Var (mkGlobalVar ("Custom" :| []) "+" intType)
        let expr = App (App customPlus (Lit (Int 1))) (Lit (Int 2))
        constantFold expr `shouldBe` expr

betaReduceTests :: Spec
betaReduceTests = describe "Beta reduction" $ do
    it "reduces simple application" $ do
        -- (\x -> x) 5 => 5
        let x = mkVar "x" intType
        let expr = App (Lam x (Var x)) (Lit (Int 5))
        betaReduce expr `shouldBe` Lit (Int 5)

    it "reduces with body referencing parameter" $ do
        -- (\x -> x + 1) 5 => 5 + 1
        let x = mkVar "x" intType
        let body = App (App (preludeOp "+") (Var x)) (Lit (Int 1))
        let expr = App (Lam x body) (Lit (Int 5))
        let expected = App (App (preludeOp "+") (Lit (Int 5))) (Lit (Int 1))
        betaReduce expr `shouldBe` expected

    it "does not reduce non-applied lambdas" $ do
        -- \x -> x stays as is
        let x = mkVar "x" intType
        let expr = Lam x (Var x)
        betaReduce expr `shouldBe` expr

    it "reduces nested beta redex" $ do
        -- (\x -> \y -> x) 1 2 => 1
        -- First: (\x -> \y -> x) 1 => \y -> 1
        -- Then: (\y -> 1) 2 => 1
        let x = mkVar "x" intType
        let y = mkVar "y" intType
        let expr = App (App (Lam x (Lam y (Var x))) (Lit (Int 1))) (Lit (Int 2))
        betaReduce expr `shouldBe` Lit (Int 1)

uselessLetInlineTests :: Spec
uselessLetInlineTests = describe "Useless let inlining" $ do
    it "inlines trivial let binding" $ do
        -- let x = 5 in x => 5
        let x = mkVar "x" intType
        let expr = Let (NonRecursive (x, Lit (Int 5))) (Var x)
        uselessLetInline expr `shouldBe` Lit (Int 5)

    it "does not inline when body differs from binding" $ do
        -- let x = 5 in y should stay
        let x = mkVar "x" intType
        let y = mkVar "y" intType
        let expr = Let (NonRecursive (x, Lit (Int 5))) (Var y)
        uselessLetInline expr `shouldBe` expr

    it "does not inline recursive bindings" $ do
        -- let rec x = f x in x stays
        let x = mkVar "x" intType
        let f = mkVar "f" (Core.FuncTy intType intType)
        let expr = Let (Recursive [(x, App (Var f) (Var x))]) (Var x)
        uselessLetInline expr `shouldBe` expr

pipeInlineTests :: Spec
pipeInlineTests = describe "Pipe inlining" $ do
    it "inlines pipe operator" $ do
        -- x |> f => f x
        let x = mkVar "x" intType
        let f = mkVar "f" (Core.FuncTy intType intType)
        let expr = App (App (primOp "|>") (Var x)) (Var f)
        pipeInline expr `shouldBe` App (Var f) (Var x)

    it "does not inline non-Prim pipe" $ do
        -- Custom.|> x f stays
        let customPipe = Var (mkGlobalVar ("Custom" :| []) "|>" intType)
        let x = mkVar "x" intType
        let f = mkVar "f" (Core.FuncTy intType intType)
        let expr = App (App customPipe (Var x)) (Var f)
        pipeInline expr `shouldBe` expr

substTests :: Spec
substTests = describe "Substitution" $ do
    it "substitutes matching variable" $ do
        let x = mkVar "x" intType
        subst x (Lit (Int 42)) (Var x) `shouldBe` Lit (Int 42)

    it "does not substitute non-matching variable" $ do
        let x = mkVar "x" intType
        let y = mkVar "y" intType
        subst x (Lit (Int 42)) (Var y) `shouldBe` Var y

    it "substitutes inside App" $ do
        let x = mkVar "x" intType
        let f = mkVar "f" (Core.FuncTy intType intType)
        subst x (Lit (Int 42)) (App (Var f) (Var x))
            `shouldBe` App (Var f) (Lit (Int 42))

composedPassTests :: Spec
composedPassTests = describe "Composed passes" $ do
    it "coreToCoreExpr combines all passes" $ do
        -- (\x -> x) (1 + 2) => 3
        let x = mkVar "x" intType
        let addExpr = App (App (preludeOp "+") (Lit (Int 1))) (Lit (Int 2))
        let expr = App (Lam x (Var x)) addExpr
        coreToCoreExpr expr `shouldBe` Lit (Int 3)

    it "fullCoreToCoreExpr reaches fixpoint" $ do
        -- let x = (\y -> y) 5 in x => 5
        let x = mkVar "x" intType
        let y = mkVar "y" intType
        let expr = Let (NonRecursive (x, App (Lam y (Var y)) (Lit (Int 5)))) (Var x)
        fullCoreToCoreExpr expr `shouldBe` Lit (Int 5)

    it "fullCoreToCoreExpr is idempotent" $ do
        let expr = App (App (preludeOp "+") (Lit (Int 1))) (Lit (Int 2))
        let once = fullCoreToCoreExpr expr
        let twice = fullCoreToCoreExpr once
        once `shouldBe` twice
