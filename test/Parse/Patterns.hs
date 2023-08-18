module Parse.Patterns where

import Elara.AST.Generic
import Elara.Parse.Pattern (patParser)
import Hedgehog
import Orphans ()
import Parse.Common (lexAndParse, shouldFailToParse, shouldParsePattern)
import Polysemy (run)
import Polysemy.Error (runError)
import Print (debugPretty, showPrettyUnannotated)
import Test.Hspec

import Arbitrary.AST (genPattern)
import Arbitrary.Literals (genDouble, genIntLiteral, genInteger)
import Arbitrary.Names (LowerAlphaText (getAlphaText), genLowerAlphaText)
import Elara.AST.Name
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec.Hedgehog

arbitraryPattern :: Spec
arbitraryPattern = it "Arbitrary patterns parse prettyPrinted" $ hedgehog $ do
    expr <- forAll genPattern
    tripping expr showPrettyUnannotated (fmap stripPatternLocation . run . runError . lexAndParse patParser)

spec :: Spec
spec = parallel $ describe "Parses patterns correctly" $ do
    arbitraryPattern
    terminalPatterns
    consPatterns
    constructorPatterns

terminalPatterns :: Spec
terminalPatterns = parallel $ describe "Parses terminal patterns correctly" $ do
    it "Parses arbitrary var patterns correctly" $ hedgehog $ do
        expr <- getAlphaText <$> forAll genLowerAlphaText
        expr `shouldParsePattern` Pattern (VarPattern (NormalVarName $ LowerAlphaName expr), Nothing)

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
        i <- getAlphaText <$> forAll genLowerAlphaText
        show i `shouldParsePattern` Pattern (StringPattern i, Nothing)

consPatterns :: Spec
consPatterns = describe "Parses cons patterns" $ do
    it
        "Doesn't allow cons patterns without parentheses"
        (shouldFailToParse "x :: xs")

    it "Parses correct cons patterns correctly" $ hedgehog $ do
        "(x :: xs)"
            `shouldParsePattern` Pattern
                ( ConsPattern
                    (Pattern (VarPattern "x", Nothing))
                    (Pattern (VarPattern "xs", Nothing))
                , Nothing
                )

        "(x :: xs :: xss)"
            `shouldParsePattern` Pattern
                ( ConsPattern
                    (Pattern (VarPattern "x", Nothing))
                    ( Pattern
                        ( ConsPattern
                            (Pattern (VarPattern "xs", Nothing))
                            (Pattern (VarPattern "xss", Nothing))
                        , Nothing
                        )
                    )
                , Nothing
                )

constructorPatterns :: Spec
constructorPatterns = describe "Parses constructor parens" $ do
    it "Parses constructor patterns correctly" $ hedgehog $ do
        "ZeroArity" `shouldParsePattern` Pattern (ConstructorPattern "ZeroArity" [], Nothing)

    it "Fails with multiple arguments without parens" $ do
        shouldFailToParse "TwoArgs 1 2"

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
