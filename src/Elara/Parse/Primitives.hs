module Elara.Parse.Primitives (Parser, located, lineComment, sc, scn, lexeme, symbol, inParens, commaSeparated, oneOrCommaSeparatedInParens, skipNewlines, withPredicate) where

import Text.Megaparsec

import Elara.AST.Region (Located (..), SourceRegion (..))
import Elara.Parse.Error
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (many, some)
import Print (debugColored)

type Parser = Parsec ElaraParseError Text

located :: Parser a -> Parser (Located a)
located p = do
    start <- getOffset
    file <- sourceName . pstateSourcePos . statePosState <$> getParserState
    x <- p
    end <- getOffset
    pure $ Located (SourceRegion (Just file) start end) x

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

sc :: Parser ()
sc = L.space hspace1 lineComment empty

scn :: Parser ()
scn = L.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

inParens :: Parser a -> Parser a
inParens = between (lexeme $ char '(') (lexeme $ char ')')

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = p `sepBy` lexeme (char ',')

oneOrCommaSeparatedInParens :: Parser a -> Parser [a]
oneOrCommaSeparatedInParens p = try (inParens (p `sepBy` lexeme (char ','))) <|> one <$> p

skipNewlines :: Parser ()
skipNewlines = void (takeWhileP (Just "newline") (== '\n'))

withPredicate :: (MonadParsec e s m) => (b -> Bool) -> (b -> e) -> m b -> m b
withPredicate f msg p = do
    o <- getOffset
    r <- p
    if f r
        then pure r
        else region (setErrorOffset o) (customFailure (msg r))
