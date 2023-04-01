module Elara.Parse.Primitives (Parser, HParser, fmapLocated, located, inParens, inBraces, commaSeparated, oneOrCommaSeparatedInParens, token, token', withPredicate, (<??>), IsParser (..), satisfyMap, lexeme, locatedTokens') where

import Text.Megaparsec hiding (Token, token)
import Text.Megaparsec qualified as MP (token)

import Control.Lens
import Elara.AST.Region
import Elara.Lexer.Token
import Elara.Parse.Error ( ElaraParseError )
import Elara.Parse.Stream (TokenStream (tokenStreamTokens))
import HeadedMegaparsec qualified as H
import Prelude hiding (many, some)

type Parser = Parsec ElaraParseError TokenStream
type HParser = H.HeadedParsec ElaraParseError TokenStream

(<??>) :: HParser a -> String -> HParser a
(<??>) = flip H.label

class Monad m => IsParser m where
    toParsec :: m a -> Parser a
    fromParsec :: Parser a -> m a

instance IsParser Parser where
    toParsec = identity
    fromParsec = identity

instance IsParser HParser where
    toParsec = H.toParsec
    fromParsec = H.parse

{- | A parser that records the location information of the tokens it consumes.
TODO this is not going to perform very well as it's O(n) in the total number of input tokens
A future solution will be to store the number of tokens consumed in the 'TokenStream' and use that to calculate
the spanning region, but that's effort at the moment.
-}
located :: IsParser m => m a -> m (Located a)
located p = do
    startTokens <- tokenStreamTokens . stateInput <$> fromParsec getParserState
    val <- p
    endStream <- tokenStreamTokens . stateInput <$> fromParsec getParserState
    let diff = length startTokens - length endStream
    let tokensDiff = take diff startTokens
    let tokensRegion = case tokensDiff of
            [] -> error "empty?"
            x : xs -> spanningRegion' (view sourceRegion <$> x :| xs)
    pure $ Located tokensRegion val

fmapLocated :: IsParser f => (Located a -> b) -> f a -> f b
fmapLocated f = (f <$>) . located

token :: IsParser m => Token -> m Token
token = fmap (view unlocated) . lexeme

lexeme :: IsParser m => Token -> m Lexeme
lexeme = fromParsec . singleToken
  where
    singleToken :: Token -> Parser Lexeme
    singleToken t = MP.token (test t) []
    test :: Token -> Lexeme -> Maybe Lexeme
    test t l@(Located _ t2) | t == t2 = Just l
    test _ _ = Nothing

satisfyMap :: forall m a. IsParser m => (Token -> Maybe a) -> m a
satisfyMap f = fromParsec $ MP.token test []
  where
    test :: Lexeme -> Maybe a
    test (Located _ t) = f t

token' :: IsParser m => Token -> m ()
token' = void . token

locatedTokens' :: IsParser m => NonEmpty Token -> m SourceRegion
locatedTokens' tokenList = do
    ts <- traverse lexeme tokenList
    pure $ spanningRegion' (view sourceRegion <$> ts)

inParens :: HParser a -> HParser a
inParens = surroundedBy (token' TokenLeftParen) (token' TokenRightParen)

inBraces :: HParser a -> HParser a
inBraces = surroundedBy (token' TokenLeftBrace) (token' TokenRightBrace)

surroundedBy :: Monad m => m a1 -> m a2 -> m b -> m b
surroundedBy before after p = do
    _ <- before
    x <- p
    _ <- after
    pure x

commaSeparated :: HParser a -> HParser [a]
commaSeparated p = p `sepBy` token' TokenComma

oneOrCommaSeparatedInParens :: HParser a -> HParser [a]
oneOrCommaSeparatedInParens p = inParens (p `sepBy` token' TokenComma) <|> one <$> p

withPredicate :: IsParser m => (t -> Bool) -> (t -> ElaraParseError) -> m t -> m t
withPredicate f msg p = do
    o <- fromParsec getOffset
    r <- p
    if f r
        then pure r
        else fromParsec $ region (setErrorOffset o) (fromParsec $ customFailure (msg r))
