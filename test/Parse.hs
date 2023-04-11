{-# LANGUAGE ViewPatterns #-}

module Parse where

import Elara.AST.StripLocation
import Elara.AST.Unlocated.Frontend as Unlocated
import Elara.AST.Frontend qualified as Frontend
import Elara.Data.Pretty

import Elara.Parse.Expression (exprParser)
import Lex.Common
import Parse.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Elara.Parse.Stream
import Print
import Arbitrary.AST

spec :: Spec
spec = do
    quickCheckSpec

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
    let
        source = showPretty $ pretty expr
        lexed = lex' source
        parsed = parse exprParser (TokenStream (toString source) lexed)
        cleaned = removeInParens . (stripLocation @Frontend.Expr @Unlocated.Expr) <$> parsed
     in
        counterexample (toString source) (cleaned `shouldParseProp` expr)


