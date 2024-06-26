module Parse.Patterns where

import Arbitrary.AST (genPattern)
import Arbitrary.Literals (genDouble, genInteger)
import Arbitrary.Names (genLowerAlphaText)
import Elara.AST.Generic
import Elara.AST.Name
import Elara.AST.StripLocation
import Elara.Parse.Pattern (patParser)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Orphans ()
import Parse.Common (lexAndParse, shouldFailToParse, shouldParsePattern, trippingParse)
import Print (showPrettyUnannotated)
import Test.Hspec
import Test.Hspec.Hedgehog

arbitraryPattern :: Spec
arbitraryPattern = it "Arbitrary patterns parse prettyPrinted" $ hedgehog $ do
    expr <- forAll genPattern
    let parsePretty s = fmap stripLocation <$> lexAndParse patParser s
    trippingParse expr showPrettyUnannotated parsePretty

spec :: Spec
spec = parallel $ describe "Parses patterns correctly" $ do
    arbitraryPattern
    terminalPatterns
    consPatterns
    constructorPatterns

terminalPatterns :: Spec
terminalPatterns = parallel $ describe "Parses terminal patterns correctly" $ do
    it "Parses arbitrary var patterns correctly" $ hedgehog $ do
        expr <- forAll genLowerAlphaText
        expr `shouldParsePattern` Pattern (VarPattern (LowerAlphaName expr), Nothing)

    it "Parses wildcard pattern correctly" $ hedgehog $ do
        "_" `shouldParsePattern` Pattern (WildcardPattern, Nothing)

    it "Parses unit pattern correctly" $ hedgehog $ do
        "()" `shouldParsePattern` Pattern (UnitPattern, Nothing)

    it "Parses arbitrary int literal patterns correctly" $ hedgehog $ do
        i <- forAll genInteger
        show i `shouldParsePattern` Pattern (IntegerPattern i, Nothing)

    it "Parses arbitrary float literal patterns correctly" $ hedgehog $ do
        i <- forAll genDouble
        show i `shouldParsePattern` Pattern (FloatPattern i, Nothing)

    it "Parses arbitrary char literal patterns correctly" $ hedgehog $ do
        i <- forAll Gen.unicode
        show i `shouldParsePattern` Pattern (CharPattern i, Nothing)

    it "Parses arbitrary string literal patterns correctly" $ hedgehog $ do
        i <- forAll genLowerAlphaText
        show i `shouldParsePattern` Pattern (StringPattern i, Nothing)

consPatterns :: Spec
consPatterns = describe "Parses cons patterns" $ do
    it
        "Doesn't allow cons patterns without parentheses"
        $ hedgehog
            (shouldFailToParse "x :: xs")

    it "Parses correct cons patterns correctly" $ hedgehog $ do
        "(x :: xs)"
            `shouldParsePattern` Pattern
                ( ConsPattern
                    ( Pattern (VarPattern "x", Nothing)
                    , Pattern (VarPattern "xs", Nothing)
                    )
                , Nothing
                )

        "(x :: xs :: xss)"
            `shouldParsePattern` Pattern
                ( ConsPattern
                    ( Pattern (VarPattern "x", Nothing)
                    , Pattern
                        ( ConsPattern
                            ( Pattern (VarPattern "xs", Nothing)
                            , Pattern (VarPattern "xss", Nothing)
                            )
                        , Nothing
                        )
                    )
                , Nothing
                )

constructorPatterns :: Spec
constructorPatterns = describe "Parses constructor parens" $ do
    it "Parses constructor patterns correctly" $ hedgehog $ do
        "ZeroArity" `shouldParsePattern` Pattern (ConstructorPattern "ZeroArity" [], Nothing)

    it "Parses single constructor pattern without parens" $ hedgehog $ do
        "TwoArgs 1 2"
            `shouldParsePattern` Pattern
                ( ConstructorPattern
                    "TwoArgs"
                    [Pattern (IntegerPattern 1, Nothing), Pattern (IntegerPattern 2, Nothing)]
                , Nothing
                )

    it "Parses constructor patterns with parens correctly" $ hedgehog $ do
        "(TwoArgs one two)"
            `shouldParsePattern` Pattern
                ( ConstructorPattern
                    "TwoArgs"
                    [Pattern (VarPattern "one", Nothing), Pattern (VarPattern "two", Nothing)]
                , Nothing
                )

    it "Parses nested constructor patterns correctly" $ hedgehog $ do
        "(TwoArgs (OneArg one) (OneArg two))"
            `shouldParsePattern` Pattern
                ( ConstructorPattern
                    "TwoArgs"
                    [ Pattern
                        ( ConstructorPattern
                            "OneArg"
                            [Pattern (VarPattern "one", Nothing)]
                        , Nothing
                        )
                    , Pattern
                        ( ConstructorPattern
                            "OneArg"
                            [Pattern (VarPattern "two", Nothing)]
                        , Nothing
                        )
                    ]
                , Nothing
                )
