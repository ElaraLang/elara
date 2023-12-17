module Parse.Expressions where

import Arbitrary.AST (genExpr)
import Common (stripInParens)
import Elara.AST.Generic
import Elara.AST.Generic.Pattern
import Elara.AST.Name
import Elara.AST.StripLocation
import Elara.Parse.Expression (exprParser)
import Hedgehog hiding (Var)
import Orphans ()
import Parse.Common
import Print (showPrettyUnannotated)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = parallel $ describe "Parses expressions correctly" $ do
    arbitraryExpr
    weirdEdgeCases

weirdEdgeCases :: Spec
weirdEdgeCases = describe "Parses some weird edge cases correctly" $ do
    it "Parses the funky lambda thing properly" $ hedgehog $ do
        "(\\x -> x + 2) 3"
            `shouldParseExpr` functionCall
                ( Expr
                    ( InParens
                        ( Expr
                            ( Lambda
                                [Pattern (VarPattern (LowerAlphaName "x"), Nothing)]
                                ( Expr
                                    ( BinaryOperator
                                        ( MkBinaryOperator (SymOp "+")
                                        , Expr (Var (MaybeQualified "x" Nothing), Nothing)
                                        , Expr (Int 2, Nothing)
                                        )
                                    , Nothing
                                    )
                                )
                            , Nothing
                            )
                        )
                    , Nothing
                    )
                )
                (Expr (Int 3, Nothing))
    it "Parses the weird let-in thing properly" $ hedgehog $ do
        "let a  = 0 in {let a  = -98905857 }"
            `shouldParseExpr` Expr
                ( LetIn
                    "a"
                    []
                    (Expr (Int 0, Nothing))
                    ( Expr
                        ( Let
                            (NormalVarName (LowerAlphaName "a"))
                            []
                            (Expr (Int (-98905857), Nothing))
                        , Nothing
                        )
                    )
                , Nothing
                )

arbitraryExpr :: Spec
arbitraryExpr = it "Arbitrary expressions parse prettyPrinted" $ hedgehog $ do
    expr <- forAll genExpr
    let parsePretty s = fmap stripLocation <$> lexAndParse exprParser s

    trippingParse expr showPrettyUnannotated (fmap (fmap stripInParens) . parsePretty)
