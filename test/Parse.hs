{-# LANGUAGE ViewPatterns #-}

module Parse where

import Arbitrary.AST ()
import Elara.AST.Generic
import Elara.AST.Select
import Elara.Data.Pretty
import Elara.Parse.Expression (exprParser)
import Elara.Parse.Stream
import Lex.Common
import Parse.Common
import Print (showPretty, showPrettyUnannotated)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
    -- quickCheckSpec
    pass

quickCheckSpec :: Spec
quickCheckSpec = modifyMaxSize (const 5) $ prop "Arbitrary expressions parse prettyPrinted" ppEq

removeInParens :: Expr 'UnlocatedFrontend -> Expr 'UnlocatedFrontend
removeInParens (Expr (Lambda p e, t)) = Expr (Lambda p (removeInParens e), t)
removeInParens (Expr (FunctionCall e1 e2, t)) = Expr (FunctionCall (removeInParens e1) (removeInParens e2), t)
removeInParens (Expr (If e1 e2 e3, t)) = Expr (If (removeInParens e1) (removeInParens e2) (removeInParens e3), t)
removeInParens (Expr (BinaryOperator op e1 e2, t)) = Expr (BinaryOperator op (removeInParens e1) (removeInParens e2), t)
removeInParens (Expr (List es, t)) = Expr (List (removeInParens <$> es), t)
removeInParens (Expr (LetIn v ps e1 e2, t)) = Expr (LetIn v ps (removeInParens e1) (removeInParens e2), t)
removeInParens (Expr (Let v ps e, t)) = Expr (Let v ps (removeInParens e), t)
removeInParens (Expr (Block es, t)) = Expr (Block (removeInParens <$> es), t)
removeInParens (Expr (InParens e, _)) = removeInParens e
removeInParens e = e

ppEq :: Expr 'UnlocatedFrontend -> Property
ppEq (removeInParens -> expr) =
    let source = showPrettyUnannotated $ pretty expr
        lexed = lex' source
        parsed = parse exprParser (TokenStream (toString source) lexed 0)
        cleaned = removeInParens . stripExprLocation <$> parsed
     in counterexample (toString $ showPretty $ pretty source) (cleaned `shouldParseProp` expr)
