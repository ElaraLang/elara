-- | Operator shunting (precedence resolution) tests
module Shunt (spec) where

import Boilerplate (loadShuntedExprIO, pipelineResShouldSucceed)
import Elara.AST.New.Phases.Shunted (ShuntedExpr, ShuntedExpr')
import Elara.AST.New.Types qualified as New
import Test.Syd

-- | Extract the 'ShuntedExpr'' from a 'ShuntedExpr'
unExpr :: ShuntedExpr -> ShuntedExpr'
unExpr (New.Expr _ _ e') = e'

-- | Check that an expression is a function call and return @(fn, arg)@ if so
asFunctionCall :: ShuntedExpr -> Maybe (ShuntedExpr, ShuntedExpr)
asFunctionCall e = case unExpr e of
    New.EApp _ fn arg -> Just (fn, arg)
    _ -> Nothing

-- | Check that an expression is an integer literal with a specific value
isInt :: Integer -> ShuntedExpr -> Bool
isInt n e = case unExpr e of
    New.EInt m -> n == m
    _ -> False

spec :: Spec
spec = describe "Shunts operators correctly" $ do
    it "Shunts simple operators into prefix calls" $ do
        -- 1 + 2 should become ((+) 1) 2
        expr <- pipelineResShouldSucceed $ loadShuntedExprIO "1 + 2"
        -- expr == FunctionCall ((+) 1) 2
        case asFunctionCall expr of
            Just (plusOne, two) -> do
                isInt 2 two `shouldBe` True
                -- plusOne == FunctionCall (+) 1
                case asFunctionCall plusOne of
                    Just (plusVar, one) -> do
                        isInt 1 one `shouldBe` True
                        -- The innermost should be a Var (the + operator)
                        case unExpr plusVar of
                            New.EVar _ _ -> pass
                            other -> expectationFailure $ "Expected Var for operator, got: " ++ show other
                    Nothing -> expectationFailure "Expected inner FunctionCall for curried operator"
            Nothing -> expectationFailure "Expected FunctionCall at top level"

    it "Shunts repeated operators with left associativity" $ do
        -- 1 + 2 + 3 with left assoc should become ((+) ((+) 1 2) 3)
        expr <- pipelineResShouldSucceed $ loadShuntedExprIO "1 + 2 + 3"
        -- expr == FunctionCall (FunctionCall (+) ((+) 1 2)) 3
        case asFunctionCall expr of
            Just (inner, three) -> do
                isInt 3 three `shouldBe` True
                -- inner should be: FunctionCall (+) ((+) 1 2)
                case asFunctionCall inner of
                    Just (_plusVar, onePlusTwo) -> do
                        -- onePlusTwo should be: FunctionCall (FunctionCall (+) 1) 2
                        case asFunctionCall onePlusTwo of
                            Just (_, two) -> isInt 2 two `shouldBe` True
                            Nothing -> expectationFailure "Expected FunctionCall for (1 + 2)"
                    Nothing -> expectationFailure "Expected curried application"
            Nothing -> expectationFailure "Expected FunctionCall at top level"

    it "Respects higher precedence of * over +" $ do
        -- 1 + 2 * 3 should become ((+) 1 ((*) 2 3))
        -- Because * binds tighter, it should be the inner call
        expr <- pipelineResShouldSucceed $ loadShuntedExprIO "1 + 2 * 3"
        -- expr == FunctionCall (FunctionCall (+) 1) ((*) 2 3)
        case asFunctionCall expr of
            Just (plusOne, timesExpr) -> do
                -- Right side should be (* 2 3), i.e. FunctionCall (FunctionCall (*) 2) 3
                case asFunctionCall timesExpr of
                    Just (_, three) -> isInt 3 three `shouldBe` True
                    Nothing -> expectationFailure "Expected (* 2 3) on right side of +"
                -- Left side should be (+ 1)
                case asFunctionCall plusOne of
                    Just (_, one) -> isInt 1 one `shouldBe` True
                    Nothing -> expectationFailure "Expected (+ 1) on left side"
            Nothing -> expectationFailure "Expected FunctionCall at top level"

    it "Respects parentheses overriding precedence" $ do
        -- (1 + 2) * 3 should become ((*) ((+) 1 2) 3)
        -- \* is the outermost call, with (+ 1 2) as argument
        expr <- pipelineResShouldSucceed $ loadShuntedExprIO "(1 + 2) * 3"
        -- expr == FunctionCall (FunctionCall (*) (+ 1 2)) 3
        case asFunctionCall expr of
            Just (timesInner, three) -> do
                isInt 3 three `shouldBe` True
                -- timesInner: FunctionCall (*) (+ 1 2)
                case asFunctionCall timesInner of
                    Just (_, plusExpr) -> do
                        -- plusExpr should be (+ 1 2)
                        case asFunctionCall plusExpr of
                            Just (_, two) -> isInt 2 two `shouldBe` True
                            Nothing -> expectationFailure "Expected (+ 1 2) inside (* ...)"
                    Nothing -> expectationFailure "Expected curried * application"
            Nothing -> expectationFailure "Expected FunctionCall at top level"

    it "Handles equality operator at lower precedence" $ do
        -- 1 == 2 should become ((==) 1 2)
        expr <- pipelineResShouldSucceed $ loadShuntedExprIO "1 == 2"
        case asFunctionCall expr of
            Just (eqOne, two) -> do
                isInt 2 two `shouldBe` True
                case asFunctionCall eqOne of
                    Just (_, one) -> isInt 1 one `shouldBe` True
                    Nothing -> expectationFailure "Expected curried == application"
            Nothing -> expectationFailure "Expected FunctionCall at top level"
