module Elara.Parse.Primitives (Parser, HParser, fmapLocated, located, lineComment, sc, scn, lexeme, symbol, inParens, commaSeparated, oneOrCommaSeparatedInParens, skipNewlines, withPredicate, (<??>), IsParser (..), char, char', skipSpaces) where

import Text.Megaparsec

import Elara.AST.Region (Located (..), SourceRegion (..))
import Elara.Parse.Error
import HeadedMegaparsec qualified as H
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (many, some)

type Parser = Parsec ElaraParseError Text
type HParser = H.HeadedParsec ElaraParseError Text

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
    start <- fromParsec getOffset
    file <- sourceName . pstateSourcePos . statePosState <$> fromParsec getParserState
    x <- p
    end <- fromParsec getOffset
    pure $ Located (SourceRegion (Just file) start end) x

fmapLocated :: IsParser f => (Located a -> b) -> f a -> f b
fmapLocated f = (f <$>) . located

lineComment :: IsParser m => m ()
lineComment = fromParsec $ L.skipLineComment "--"

sc :: IsParser m => m ()
sc = fromParsec $ L.space MC.hspace1 lineComment empty

scn :: Parser ()
scn = L.space MC.space1 lineComment empty

lexeme :: IsParser m => m a -> m a
lexeme = fromParsec . L.lexeme sc . toParsec

symbol :: IsParser m => Text -> m ()
symbol = fromParsec . void . L.symbol sc

inParens :: HParser a -> HParser a
inParens p = do
    H.parse $ symbol "("
    H.endHead
    x <- p
    H.parse $ symbol ")"
    pure x

char :: IsParser m => Char -> m Char
char = fromParsec . MC.char

char' :: IsParser m => Char -> m ()
char' = void . char

commaSeparated :: HParser a -> HParser [a]
commaSeparated p = p `sepBy` lexeme (char ',')

oneOrCommaSeparatedInParens :: HParser a -> HParser [a]
oneOrCommaSeparatedInParens p = inParens (p `sepBy` lexeme (char ',')) <|> one <$> p

skipNewlines :: IsParser m => m ()
skipNewlines = fromParsec $ void (takeWhileP (Just "newline") (== '\n'))

skipSpaces :: IsParser m => m ()
skipSpaces = fromParsec $ void $ many MC.space1

withPredicate :: IsParser m => (t -> Bool) -> (t -> ElaraParseError) -> m t -> m t
withPredicate f msg p = do
    o <- fromParsec getOffset
    r <- p
    if f r
        then pure r
        else fromParsec $ region (setErrorOffset o) (fromParsec $ customFailure (msg r))
