{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

{- | Test suite that checks that the TextMate grammar (@grammar.ts@) lexes every character of generated sources the same way the
lexer does.

This is a basic property test: we synthesise arbitrary strings that match the grammar, and then verify
@lex_alex(str) is success => lex_alex(str) ≈ lex_textmate(str)@
Specifically, we define @≈@ to ignore things like spacing and layout, which the TextMate grammar doesn't bother to classify.
We use Hedgehog's 'cover' function to enforce that every token gets synthesised at least some proportion of the time (in case our generators are buggy).

These tests run automatically with @nix flake check@, or can be run manually with @just test-grammar@.
-}
module Main (main) where

import Data.Aeson (eitherDecodeStrict, encode)
import Data.Char (isSpace)
import Data.List (zipWith3)
import Effectful (runPureEff)
import Effectful.Error.Static (runError)
import Generic.Data (conIdEnum, conIdToString, gconName)
import Hedgehog (Gen, LabelName, check, cover, discard, evalIO, forAll, property, withConfidence, withTests, (===))
import Hedgehog.Internal.Property (LabelName (LabelName))
import Optics (foldMapOf)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), proc, withCreateProcess)

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Arbitrary.Literals (genCharLiteral, genFloatLiteral, genIntLiteral, genStringLiteral, unCharLiteral, unFloatLiteral, unIntLiteral, unStringLiteral)
import Arbitrary.Names (genLowerAlphaText, genModuleName, genOpText, genUpperAlphaText)
import Elara.AST.Name (nameText)
import Elara.AST.Region (Located (Located), RealSourceRegion, column, endPos, line, sourceRegion, startPos, unlocated, _RealSourceRegion)
import Elara.Lexer.Reader (readTokensWith)
import Elara.Lexer.Token (Lexeme, Token (..), isIndent, tokenRepr)
import Elara.Lexer.Utils (LexerError)
import Elara.Logging (ignoreStructuredDebug)
import Elara.ReadFile (FileContents (FileContents))

-- | Rough categorisation of what a character/token represents.
data Class = Keyword | Operator | Definition | Number | Char | String | Comment | Type | Annotation | Wildcard | Plain
    deriving (Eq, Show)

{- | The classes a token may represent.
This function may return @[]@ to mark tokens this test skips (e.g. whitespace or unimplemented tokens), so seeing them _at all_ implies a mismatch
-}
expectedClasses :: Token -> [Class]
expectedClasses = \case
    (TokenLet; TokenLetBang; TokenDef; TokenData; TokenClass; TokenType; TokenAlias) -> [Keyword]
    (TokenIf; TokenThen; TokenElse; TokenIn; TokenMatch; TokenWith) -> [Keyword]
    (TokenModule; TokenImport; TokenQualified; TokenHiding) -> [Keyword]
    (TokenRightArrow; TokenLeftArrow; TokenDoubleRightArrow; TokenBackslash; TokenPipe) -> [Operator]
    (TokenColon; TokenDoubleColon; TokenDot; TokenEquals; TokenAt) -> [Operator]
    TokenUnderscore -> [Wildcard]
    (TokenInt _; TokenFloat _) -> [Number]
    TokenChar _ -> [Char]
    TokenString _ -> [String]
    TokenVariableIdentifier _ -> [Plain, Definition]
    TokenQVariableIdentifier _ -> [Plain, Annotation]
    (TokenConstructorIdentifier _; TokenQConstructorIdentifier _) -> [Type, Annotation]
    (TokenOperatorIdentifier _; TokenQOperatorIdentifier _) -> [Operator, Definition]
    TokenHash -> [Annotation, Operator]
    (TokenSemicolon; TokenComma; TokenLeftParen; TokenRightParen; TokenLeftBracket; TokenRightBracket; TokenLeftBrace; TokenRightBrace; TokenBacktick) -> [Plain]
    (TokenIndent; TokenDedent; TokenLineSeparator; TokenEOF) -> []
    -- not produced by the lexer yet
    (TokenCase; TokenOf; TokenAs; TokenExposing; TokenWhere; TokenForall; TokenInstance; TokenDeriving) -> []

{- | Tests a list of the TextMate scopes to find the first matching prefix of how we can classify a character/token
>>> scopeClass [TextMateScope "DoesntMatter", TextMateScope "keyword.operator"]
Operator
-}
scopeClass :: [TextMateScope] -> Class
scopeClass scopes = maybe Plain snd (find (\(prefix, _) -> any ((prefix `T.isPrefixOf`) . coerce) scopes) prefixes)
  where
    prefixes =
        [ ("comment.", Comment)
        , ("string.quoted.double", String)
        , ("string.quoted.single", Char)
        , ("constant.numeric", Number)
        , ("entity.other.attribute-name", Annotation)
        , ("entity.name.function", Definition)
        , ("keyword.operator", Operator)
        , ("keyword.", Keyword)
        , ("variable.language.wildcard", Wildcard)
        , ("support.type", Type)
        ]

{- | A @vscode-textmate@ tokenising result.
All numbers 0-based and end-exclusive.
-}
data TextMateResult = TextMateResult
    { start :: Int
    , end :: Int
    , scopes :: [TextMateScope]
    }

resultFromTuple :: (Int, Int, [Text]) -> TextMateResult
resultFromTuple (start, end, scopes) = TextMateResult start end (fmap TextMateScope scopes)

newtype TextMateScope = TextMateScope Text deriving (Eq, Show)

-- | The output from @tokenise.ts@, a list of tokens on each line
type TokeniseResult = [[TextMateResult]]

-- | Create the @tokenise.ts@ process that tokenises some input string with @vscode-textmate@.
withTokenizer :: ((Text -> IO TokeniseResult) -> IO a) -> IO a
withTokenizer use =
    withCreateProcess (proc "bun" ["editors/textmate/tokenise.ts"]){std_in = CreatePipe, std_out = CreatePipe} $ \case
        Just stdin -> \case
            Just stdout -> \_ _ -> use $ \source -> do
                LBS.hPut stdin (encode source <> "\n")
                hFlush stdin

                output <- BS8.hGetLine stdout
                let decodedAsTuple = eitherDecodeStrict @[[(Int, Int, [Text])]] output
                case decodedAsTuple of
                    Left err -> fail err
                    Right results -> pure (resultFromTuple <<$>> results)
            Nothing -> error "no stdout pipe"
        Nothing -> error "no stdin pipe"

{- | Fully lex a source string, filtering out any layout tokens, or returning 'Nothing'
if lexing fails.
-}
lexSource :: Text -> Maybe [Lexeme]
lexSource source = do
    let lexemes =
            rightToMaybe
                ( runPureEff
                    ( runError @LexerError
                        ( ignoreStructuredDebug
                            (readTokensWith (FileContents "generated" source))
                        )
                    )
                )
    filter (\(Located _ t) -> not (isIndent t)) <$> lexemes

-- | Describes a mismatch between the TextMate grammar and main lexer
data Mismatch = Mismatch
    { line :: Int
    , column :: Int
    , char :: Char
    , expected :: [Class]
    , got :: [TextMateScope]
    }
    deriving (Eq, Show)

-- | Returns every non-space character that the TextMate grammar classifies differently from the Alex lexer.
mismatches ::
    -- | The original source string
    Text ->
    -- | The lexemes produced by the lexer
    [Lexeme] ->
    -- | The tokenised output produced by the grammar
    TokeniseResult ->
    -- | A list of mismatches
    [Mismatch]
mismatches source lexemes tokenised =
    concat (zipWith3 lineMismatches [1 ..] (T.splitOn "\n" source) tokenised)
  where
    lineMismatches l text grammarTokens =
        mapMaybe
            (uncurry (charMismatch l grammarTokens))
            (zip [1 ..] (toString text))

    charMismatch :: Int -> [TextMateResult] -> Int -> Char -> Maybe Mismatch
    charMismatch l grammarTokens c ch = do
        guard (not (isSpace ch)) -- we don't care if there's a spacing mismatch since TM isn't layout sensitive
        let allowedClasses = Map.findWithDefault [Comment] (l, c) expectedClassesOfPositions
            textMateScopes = maybe [] (.scopes) (find (\t -> t.start < c && c <= t.end) grammarTokens)
        guard (scopeClass textMateScopes `notElem` allowedClasses) -- there's only a mismatch if the grammar produced something that the lexer doesn't allow
        pure (Mismatch l c ch allowedClasses textMateScopes)

    expectedClassesOfPositions = Map.fromList (concatMap lexemeColumns lexemes)

    lexemeColumns :: Lexeme -> [((Int, Int), [Class])]
    lexemeColumns lexeme =
        foldMapOf
            (sourceRegion % _RealSourceRegion)
            (regionColumns (lexeme ^. unlocated))
            lexeme

    regionColumns :: Token -> RealSourceRegion -> [((Int, Int), [Class])]
    regionColumns t region =
        fmap
            (\c -> ((region ^. startPos % line, c), expectedClasses t))
            [region ^. startPos % column .. (region ^. endPos % column) - 1]

constructorName :: Token -> Text
constructorName = toText . gconName

-- | Tokens the lexer shouldn't produce for the purposes of this test: layout, and unimplemented tokens.
unlexed :: [Token]
unlexed =
    [TokenIndent, TokenDedent, TokenLineSeparator, TokenEOF]
        <> [TokenCase, TokenOf, TokenAs, TokenExposing, TokenWhere, TokenForall, TokenInstance, TokenDeriving]

{- | Given a list of lexemes, produces a coverage table of:
The constructor name of every known token that is not 'unlexed', and whether the list of lexemes contains it at all

>>> take 3 (tokenCases [TokenSemicolon])
[(LabelName {unLabelName = "TokenSemicolon"},True),(LabelName {unLabelName = "TokenComma"},False),(LabelName {unLabelName = "TokenDot"},False)]
-}
tokenCases :: [Token] -> [(LabelName, Bool)]
tokenCases lexemes =
    [ (LabelName (toString name), name `elem` present)
    | name <- toText . conIdToString <$> conIdEnum @Token
    , name `notElem` (constructorName <$> unlexed)
    ]
  where
    present = ordNub (constructorName <$> lexemes)

{- | Generate arbitrary Text that lexes as some token.
>>> replicateM 10 (Gen.sample genFragment)
["let!","\\","Ejoy.>","Mqa.uxjc","#Ck.dvbv","with","if","let","<-",":"]
-}
genFragment :: Gen Text
genFragment =
    Gen.frequency $
        [ (10, Gen.element (tokenRepr <$> fixedTokens))
        , (10, Gen.element punctuation)
        ]
            <> fmap (1,) (literals <> names <> [annotation, definition])
  where
    punctuation = words "\\ | . = @ ( ) [ ] { } , ; ` #"
    literals =
        [ unIntLiteral <$> genIntLiteral
        , unFloatLiteral <$> genFloatLiteral
        , unStringLiteral <$> genStringLiteral
        , unCharLiteral <$> genCharLiteral
        ]
    names = [genLowerAlphaText, genUpperAlphaText, genOpText] >>= \name -> [name, genQualified name]
    annotation = ("#" <>) <$> Gen.choice [genUpperAlphaText, genQualified genLowerAlphaText, genQualified genUpperAlphaText]
    definition = (<>) <$> Gen.element ["let ", "let! ", "def "] <*> Gen.choice [genLowerAlphaText, parens <$> genOpText]
    genQualified name = (\m n -> nameText m <> "." <> n) <$> genModuleName <*> name
    parens op = "(" <> op <> ")"

    fixedTokens =
        [TokenLet, TokenLetBang, TokenDef, TokenData, TokenClass, TokenType, TokenAlias]
            <> [TokenIf, TokenThen, TokenElse, TokenIn, TokenMatch, TokenWith]
            <> [TokenModule, TokenImport, TokenQualified, TokenHiding]
            <> [TokenRightArrow, TokenLeftArrow, TokenDoubleRightArrow, TokenColon, TokenDoubleColon, TokenUnderscore]

-- | Generate some arbitrary string that can separate two tokens, or sometimes nothing.
genSeparator :: Gen Text
genSeparator = Gen.element ["", "", "", " ", "  ", "\n", "\n  ", "-- c\n", "/- c -/", "/- /- c -/ -/"]

main :: IO ()
main = withTokenizer $ \tokenize -> do
    ok <- check . withTests 1000 . withConfidence (10 ^ 9) . property $ do
        -- generate some arbitrary source string by concatenating some random fragments and separators
        source <- forAll (T.concat <$> Gen.list (Range.linear 1 30) ((<>) <$> genFragment <*> genSeparator))
        let lexResult = lexSource source
        lexemes <- case lexResult of
            Nothing -> discard -- if the oracle lexer doesn't like it, ignore
            Just p -> pure p

        for_ (tokenCases (fmap (view unlocated) lexemes)) (uncurry (cover 1)) -- check some level of coverage for every generated token
        tokenized <- evalIO (tokenize source)
        mismatches source lexemes tokenized === []
    unless ok exitFailure
