{-# LANGUAGE ViewPatterns #-}

module Parse where

import Arbitrary.AST ()
import Elara.AST.StripLocation
import Elara.AST.Unlocated.Frontend as Unlocated
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

removeInParens :: Expr -> Expr
removeInParens (Lambda p e) = Lambda p (removeInParens e)
removeInParens (FunctionCall e1 e2) = FunctionCall (removeInParens e1) (removeInParens e2)
removeInParens (If e1 e2 e3) = If (removeInParens e1) (removeInParens e2) (removeInParens e3)
removeInParens (BinaryOperator op e1 e2) = BinaryOperator op (removeInParens e1) (removeInParens e2)
removeInParens (List es) = List (removeInParens <$> es)
removeInParens (LetIn v ps e1 e2) = LetIn v ps (removeInParens e1) (removeInParens e2)
removeInParens (Let v ps e) = Let v ps (removeInParens e)
removeInParens (Block es) = Block (removeInParens <$> es)
removeInParens (InParens e) = removeInParens e
removeInParens e = e

ppEq :: Unlocated.Expr -> Property
ppEq (removeInParens -> expr) =
    let source = showPrettyUnannotated $ pretty expr
        lexed = lex' source
        parsed = parse exprParser (TokenStream (toString source) lexed 0)
        cleaned = removeInParens . stripLocation <$> parsed
     in counterexample (toString $ showPretty $ pretty source) (cleaned `shouldParseProp` expr)