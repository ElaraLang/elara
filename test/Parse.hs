module Parse where

import Arbitrary.AST (genExpr)
import Elara.AST.Generic
import Elara.AST.Select
import Elara.Parse.Expression (exprParser)
import Hedgehog
import Orphans ()
import Parse.Common
import Print (showPrettyUnannotated)

import Parse.Patterns qualified as Patterns
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = parallel $ do
    Patterns.spec
    -- arbitraryExpr
    pass

arbitraryExpr :: Spec
arbitraryExpr = it "Arbitrary expressions parse prettyPrinted" $ hedgehog $ do
    expr <- forAll genExpr
    let parsePretty s = Right . removeInParens . stripExprLocation <$> (lexAndParse exprParser s >>= evalEither)
    trippingParse (removeInParens expr) (showPrettyUnannotated . removeInParens) parsePretty
  where
    -- The AST needs to have the 'InParens' element for operator shunting later, but its presence messes up the pretty printing & parsing equality
    -- This just removes any 'InParens' elements from the AST
    removeInParens :: Expr 'UnlocatedFrontend -> Expr 'UnlocatedFrontend
    removeInParens (Expr (Lambda p e, t)) = Expr (Lambda p (removeInParens e), t)
    removeInParens (Expr (FunctionCall e1 e2, t)) = Expr (FunctionCall (removeInParens e1) (removeInParens e2), t)
    removeInParens (Expr (If e1 e2 e3, t)) = Expr (If (removeInParens e1) (removeInParens e2) (removeInParens e3), t)
    removeInParens (Expr (BinaryOperator op e1 e2, t)) = Expr (BinaryOperator op (removeInParens e1) (removeInParens e2), t)
    removeInParens (Expr (List es, t)) = Expr (List (removeInParens <$> es), t)
    removeInParens (Expr (LetIn v ps e1 e2, t)) = Expr (LetIn v ps (removeInParens e1) (removeInParens e2), t)
    removeInParens (Expr (Let v ps e, t)) = Expr (Let v ps (removeInParens e), t)
    removeInParens (Expr (Block es, t)) = Expr (Block (removeInParens <$> es), t)
    removeInParens (Expr (Tuple e, t)) = Expr (Tuple (removeInParens <$> e), t)
    removeInParens (Expr (InParens e, _)) = removeInParens e
    removeInParens e = e

-- ppEq :: Property
-- ppEq = do
--     expr <- removeInParens <$> forAll
--     let source = showPrettyUnannotated $ pretty expr
--         parsed = run $ runError $ lexAndParse exprParser source
--         cleaned = removeInParens . stripExprLocation <$> parsed
--      in counterexample ("pretty source: \n" <> toString (showPretty $ pretty source)) (cleaned `shouldParseProp` expr)
