module Parse.Patterns where

import Arbitrary.Names (getAlphaText)
import Elara.AST.Generic
import Elara.AST.Name
import Elara.AST.Select (UnlocatedAST (..))
import Elara.Data.Pretty
import Elara.Parse.Pattern (patParser)
import Orphans ()
import Parse.Common (lexAndParse, shouldFailToParse, shouldParsePattern, shouldParseProp)
import Polysemy (run)
import Polysemy.Error (runError)
import Print (showPretty, showPrettyUnannotated)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.QuickCheck

import Arbitrary.AST ()

arbitraryPattern :: Spec
arbitraryPattern = modifyMaxSize (const 5) $ prop "Arbitrary patterns parse prettyPrinted" ppEq

ppEq :: Pattern 'UnlocatedFrontend -> Property
ppEq expr =
    let source = showPrettyUnannotated $ pretty expr
        parsed = run $ runError $ lexAndParse patParser source
        cleaned = stripPatternLocation <$> parsed
     in counterexample ("pretty source: " <> toString (showPretty $ pretty source)) (cleaned `shouldParseProp` expr)

spec :: Spec
spec = describe "Parses patterns correctly" $ do
    arbitraryPattern
    terminalPatterns
    consPatterns
    constructorPatterns

terminalPatterns :: Spec
terminalPatterns = describe "Parses terminal patterns correctly" $ do
    prop
        "Parses arbitrary var patterns correctly"
        ( let prop_ArbVarPatParses str = str `shouldParsePattern` Pattern (VarPattern (NormalVarName $ LowerAlphaName str), Nothing) in prop_ArbVarPatParses . getAlphaText
        )

    it "Parses wildcard pattern correctly" $ do
        "_" `shouldParsePattern` Pattern (WildcardPattern, Nothing)

    it "Parses unit pattern correctly" $ do
        "()" `shouldParsePattern` Pattern (UnitPattern, Nothing)

    prop
        "Parses arbitrary int literal patterns correctly"
        (\i -> show i `shouldParsePattern` Pattern (IntegerPattern i, Nothing))

    prop
        "Parses arbitrary float literal patterns correctly"
        (\i -> show i `shouldParsePattern` Pattern (FloatPattern i, Nothing))

    prop
        "Parses arbitrary char literal patterns correctly"
        (\i -> show i `shouldParsePattern` Pattern (CharPattern i, Nothing))

    prop
        "Parses arbitrary string literal patterns correctly"
        (\i -> show i `shouldParsePattern` Pattern (StringPattern i, Nothing))

consPatterns :: Spec
consPatterns = describe "Parses cons patterns" $ do
    it
        "Doesn't allow cons patterns without parentheses"
        (shouldFailToParse "x :: xs")

    it "Parses correct cons patterns correctly" $ do
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
    it "Parses constructor patterns correctly" $ do
        "ZeroArity" `shouldParsePattern` Pattern (ConstructorPattern "ZeroArity" [], Nothing)

    it "Fails with multiple arguments without parens" $ do
        shouldFailToParse "TwoArgs 1 2"

    it "Parses constructor patterns with parens correctly" $ do
        "(TwoArgs one two)"
            `shouldParsePattern` Pattern
                ( ConstructorPattern
                    "TwoArgs"
                    [Pattern (VarPattern "one", Nothing), Pattern (VarPattern "two", Nothing)]
                , Nothing
                )

    it "Parses nested constructor patterns correctly" $ do
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
