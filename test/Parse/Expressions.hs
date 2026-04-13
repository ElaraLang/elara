module Parse.Expressions where

import Arbitrary.AST (genExpr)
import Elara.AST.Extensions
import Elara.AST.Name
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Frontend
import Elara.AST.Phases.Frontend.Pretty ()
import Elara.AST.Types
import Elara.Parse.Grammar (exprParser)
import Hedgehog hiding (Var)
import NeatInterpolation (text)
import Normalise (mkExpr, mkPat, stripExpr, stripNewInParens)
import Orphans ()
import Parse.Common
import Print (showPrettyUnannotated)
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "Parses expressions correctly" $ do
    arbitraryExpr
    weirdEdgeCases
    lets

weirdEdgeCases :: Spec
weirdEdgeCases = describe "Parses some weird edge cases correctly" $ do
    it "Parses the funky lambda thing properly" $
        property $ do
            "(\\x -> x + 2) 3"
                `shouldParseExpr` mkExpr
                    ( EApp
                        NoExtension
                        ( mkExpr
                            ( EExtension
                                ( FrontendInParens
                                    ( InParensExpression
                                        ( mkExpr
                                            ( EExtension
                                                ( FrontendMultiLam
                                                    [mkPat (PVar (NormalVarName (LowerAlphaName "x")))]
                                                    ( mkExpr
                                                        ( EExtension
                                                            ( FrontendBinaryOperator
                                                                ( BinaryOperatorExpression
                                                                    (SymOp () (MaybeQualified "+" Nothing))
                                                                    (mkExpr (EVar NoExtension (MaybeQualified (NormalVarName (LowerAlphaName "x")) Nothing)))
                                                                    (mkExpr (EInt 2))
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        (mkExpr (EInt 3))
                    )
    it "Parses the weird let-in thing properly" $
        property $
            "let a = 0 in {let a  = -98905857 }"
                `shouldParseExpr` mkExpr
                    ( ELetIn
                        NoExtension
                        (NormalVarName (LowerAlphaName "a"))
                        (mkExpr (EInt 0))
                        ( mkExpr
                            ( ELet
                                NoExtension
                                (NormalVarName (LowerAlphaName "a"))
                                (mkExpr (EInt (-98905857)))
                            )
                        )
                    )

arbitraryExpr :: Spec
arbitraryExpr = it "Arbitrary expressions parse prettyPrinted" $ property $ do
    expr <- forAll genExpr
    let parsePretty s = fmap (stripExpr . stripNewInParens) <$> lexAndParse exprParser s
    trippingParse expr showPrettyUnannotated parsePretty

lets :: Spec
lets = describe "Parses lets correctly" $ do
    it "Parses a simple let-in correctly" $
        withTests 1 $
            property $
                "let x = 1 in x"
                    `shouldParseExpr` mkExpr
                        ( ELetIn
                            NoExtension
                            (NormalVarName (LowerAlphaName "x"))
                            (mkExpr (EInt 1))
                            (mkExpr (EVar NoExtension (MaybeQualified (NormalVarName (LowerAlphaName "x")) Nothing)))
                        )

    it "Parses a nested let correctly" $
        withTests 1 $
            property $
                [text|
        let x =
                let y =
                        1
                in y
        in x|]
                    `shouldParseExpr` mkExpr
                        ( ELetIn
                            NoExtension
                            (NormalVarName (LowerAlphaName "x"))
                            ( mkExpr
                                ( ELetIn
                                    NoExtension
                                    (NormalVarName (LowerAlphaName "y"))
                                    (mkExpr (EInt 1))
                                    (mkExpr (EVar NoExtension (MaybeQualified (NormalVarName (LowerAlphaName "y")) Nothing)))
                                )
                            )
                            (mkExpr (EVar NoExtension (MaybeQualified (NormalVarName (LowerAlphaName "x")) Nothing)))
                        )
