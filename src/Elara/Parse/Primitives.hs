module Elara.Parse.Primitives (Parser, HParser, fmapLocated, located, inParens, inBraces, commaSeparated, oneOrCommaSeparatedInParens, token, token', withPredicate, (<??>), IsParser (..), satisfyMap, lexeme, locatedTokens') where

import Text.Megaparsec hiding (Token, token)
import Text.Megaparsec qualified as MP (token)

import Control.Lens
import Elara.AST.Region
import Elara.Lexer.Lexer hiding (token)
import Elara.Lexer.Token
import Elara.Parse.Error
import Elara.Parse.Stream (TokenStream)
import HeadedMegaparsec qualified as H
import Print (debugColored)
import Prelude hiding (many, some)

type Parser = Parsec ElaraParseError TokenStream
type HParser = H.HeadedParsec ElaraParseError TokenStream

(<??>) :: HParser a -> String -> HParser a
(<??>) = flip H.label

class Monad m => IsParser m where
    toParsec :: m a -> Parser a
    fromParsec :: Parser a -> m a

instance IsParser Parser where
    toParsec = id
    fromParsec = id

instance IsParser HParser where
    toParsec = H.toParsec
    fromParsec = H.parse

located :: IsParser m => m a -> m (Located a)
located p = do
    start <- getPos
    x <- p
    end <- getPos
    pure $ Located (RealSourceRegion $ mkSourceRegion start end) x
  where
    getPos :: IsParser m => m SourcePos
    getPos = pstateSourcePos . statePosState <$> fromParsec getParserState

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
locatedTokens' tokens = do
    ts <- traverse lexeme tokens
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
