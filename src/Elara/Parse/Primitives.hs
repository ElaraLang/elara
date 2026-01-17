{-# LANGUAGE OverloadedLists #-}

module Elara.Parse.Primitives (
    Parser,
    fmapLocated,
    located,
    optionallyInParens,
    inParens,
    inBraces,
    commaSeparated,
    oneOrCommaSeparatedInParens,
    token,
    token_,
    withPredicate,
    satisfyMap,
    lexeme,
    locatedTokens',
    ignoringIndents,
)
where

import Effectful
import Elara.AST.Region
import Elara.Lexer.Token
import Elara.Logging (StructuredDebug)
import Elara.Parse.Error (ElaraParseError)
import Elara.Parse.Stream (TokenStream (..))
import Text.Megaparsec hiding (Token, token)
import Text.Megaparsec qualified as MP (token)
import Prelude hiding (many, some)

type Parser = ParsecT ElaraParseError TokenStream (Eff '[StructuredDebug])

{- | Temporarily puts the parser in a state where it ignores indents
this is useful for parsing things that are truly context-free, such as type declarations
where the indentation level is not significant.
-}
ignoringIndents :: Parser a -> Parser a
ignoringIndents p = do
    s <- getInput
    setInput (s{skipIndents = True})
    r <- p
    s' <- getInput
    setInput (s'{skipIndents = False})
    void $ optional (token_ TokenDedent) -- this can get leftover because p wouldn't have consumed it
    pure r

{- | A parser that records the location information of the tokens it consumes.
TODO this is not going to perform very well as it's O(n) in the total number of input tokens
A future solution will be to store the number of tokens consumed in the 'TokenStream' and use that to calculate
the spanning region, but that's effort at the moment.
-}
located :: Parser a -> Parser (Located a)
located p = do
    startTokens <- tokenStreamTokens . stateInput <$> getParserState
    val <- p
    endStream <- tokenStreamTokens . stateInput <$> getParserState
    let diff = length startTokens - length endStream
    let tokensDiff = take diff startTokens
    let tokensRegion = case tokensDiff of
            [] -> error "empty?"
            x : xs -> spanningRegion' (view sourceRegion <$> x :| xs)
    pure $ Located tokensRegion val

fmapLocated :: (Located a -> b) -> Parser a -> Parser b
fmapLocated f = (f <$>) . located

token :: Token -> Parser Token
token = fmap (view unlocated) . lexeme

lexeme :: Token -> Parser Lexeme
lexeme = singleToken
  where
    singleToken :: Token -> Parser Lexeme
    singleToken t = MP.token (test t) []
    test :: Token -> Lexeme -> Maybe Lexeme
    test t l@(Located _ t2) | t == t2 = Just l
    test _ _ = Nothing

satisfyMap :: forall a. (Token -> Maybe a) -> Parser a
satisfyMap f = MP.token test []
  where
    test :: Lexeme -> Maybe a
    test (Located _ t) = f t

token_ :: Token -> Parser ()
token_ = void . token

locatedTokens' :: NonEmpty Token -> Parser SourceRegion
locatedTokens' tokenList = do
    ts <- traverse lexeme tokenList
    pure $ spanningRegion' (view sourceRegion <$> ts)

inParens :: Parser a -> Parser a
inParens = between (token_ TokenLeftParen) (token_ TokenRightParen)

optionallyInParens :: Parser a -> Parser a
optionallyInParens p = inParens p <|> p

inBraces :: Parser a -> Parser a
inBraces = between (token_ TokenLeftBrace) (token_ TokenRightBrace)

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = p `sepBy` token_ TokenComma

oneOrCommaSeparatedInParens :: Parser a -> Parser [a]
oneOrCommaSeparatedInParens p = inParens (p `sepBy` token_ TokenComma) <|> one <$> p

withPredicate :: (t -> Bool) -> (t -> ElaraParseError) -> Parser t -> Parser t
withPredicate f msg p = do
    o <- getOffset
    r <- p
    if f r
        then pure r
        else region (setErrorOffset o) (customFailure (msg r))
