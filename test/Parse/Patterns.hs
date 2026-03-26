module Parse.Patterns where

import Arbitrary.AST (genPattern)
import Arbitrary.Literals (genDouble, genInteger)
import Arbitrary.Names (genLowerAlphaText)
import Elara.AST.Name
import Elara.AST.New.Extensions (ListTuplePatternExtension (..))
import Elara.AST.New.Phases.Frontend ()
import Elara.AST.New.Phases.Frontend.Pretty ()
import Elara.AST.New.Types (Pattern' (..))
import Elara.Parse.Pattern (patParser)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Normalise (mkPat, stripNewInParens, stripPattern)
import Orphans ()
import Parse.Common (lexAndParse, shouldFailToParse, shouldParsePattern, trippingParse)
import Print (showPrettyUnannotated)
import Test.Syd
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "Parses patterns correctly" $ do
    it "Arbitrary patterns parse prettyPrinted" $ property $ do
        expr <- forAll genPattern
        let parsePretty s = fmap stripPattern <$> lexAndParse patParser s
        trippingParse expr showPrettyUnannotated parsePretty

    terminalPatterns
    consPatterns
    constructorPatterns

terminalPatterns :: Spec
terminalPatterns = parallel $ describe "Parses terminal patterns correctly" $ do
    it "Parses arbitrary var patterns correctly" $ property $ do
        expr <- forAll genLowerAlphaText
        expr `shouldParsePattern` mkPat (PVar (NormalVarName (LowerAlphaName expr)))

    it "Parses wildcard pattern correctly" $ property $ "_" `shouldParsePattern` mkPat PWildcard

    it "Parses unit pattern correctly" $ property $ "()" `shouldParsePattern` mkPat PUnit

    it "Parses arbitrary int literal patterns correctly" $ property $ do
        i <- forAll genInteger
        show i `shouldParsePattern` mkPat (PInt i)

    it "Parses arbitrary float literal patterns correctly" $ property $ do
        i <- forAll genDouble
        show i `shouldParsePattern` mkPat (PFloat i)

    it "Parses arbitrary char literal patterns correctly" $ property $ do
        i <- forAll Gen.unicode
        show i `shouldParsePattern` mkPat (PChar i)

    it "Parses arbitrary string literal patterns correctly" $ property $ do
        i <- forAll genLowerAlphaText
        show i `shouldParsePattern` mkPat (PString i)

consPatterns :: Spec
consPatterns = describe "Parses cons patterns" $ do
    it "Doesn't allow cons patterns without parentheses" $
        property $
            shouldFailToParse "x :: xs"

    it "Parses correct cons patterns correctly" $ property $ do
        "(x :: xs)"
            `shouldParsePattern` mkPat
                ( PExtension
                    ( ConsPattern
                        (mkPat (PVar (NormalVarName "x")))
                        (mkPat (PVar (NormalVarName "xs")))
                    )
                )

        "(x :: xs :: xss)"
            `shouldParsePattern` mkPat
                ( PExtension
                    ( ConsPattern
                        (mkPat (PVar (NormalVarName "x")))
                        ( mkPat
                            ( PExtension
                                ( ConsPattern
                                    (mkPat (PVar (NormalVarName "xs")))
                                    (mkPat (PVar (NormalVarName "xss")))
                                )
                            )
                        )
                    )
                )

constructorPatterns :: Spec
constructorPatterns = describe "Parses constructor parens" $ do
    it "Parses constructor patterns correctly" $ property $ "ZeroArity" `shouldParsePattern` mkPat (PCon "ZeroArity" [])

    it "Parses single constructor pattern without parens" $
        property $
            "TwoArgs 1 2"
                `shouldParsePattern` mkPat
                    ( PCon
                        "TwoArgs"
                        [mkPat (PInt 1), mkPat (PInt 2)]
                    )

    it "Parses constructor patterns with parens correctly" $
        property $
            "(TwoArgs one two)"
                `shouldParsePattern` mkPat
                    ( PCon
                        "TwoArgs"
                        [ mkPat (PVar (NormalVarName "one"))
                        , mkPat (PVar (NormalVarName "two"))
                        ]
                    )

    it "Parses nested constructor patterns correctly" $
        property $
            "(TwoArgs (OneArg one) (OneArg two))"
                `shouldParsePattern` mkPat
                    ( PCon
                        "TwoArgs"
                        [ mkPat
                            ( PCon
                                "OneArg"
                                [mkPat (PVar (NormalVarName "one"))]
                            )
                        , mkPat
                            ( PCon
                                "OneArg"
                                [mkPat (PVar (NormalVarName "two"))]
                            )
                        ]
                    )
    it "Parses nested unary constructor patterns correctly" $
        property $
            "Tuple2 Nil _"
                `shouldParsePattern` mkPat
                    ( PCon
                        "Tuple2"
                        [ mkPat (PCon "Nil" [])
                        , mkPat PWildcard
                        ]
                    )
