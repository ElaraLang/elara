module Elara.Parse.Primitives where

import Prelude hiding (some, many)
import Control.Monad (void)
import Data.List
import Data.Text (Text)
import Elara.Data.Located (Located)
import Elara.Data.Located qualified as Located
import Elara.Error (Error)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    between,
    getOffset,
    sepBy,
    some,
    (<|>),
    empty
  )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Error Text

located :: Parser p -> Parser (Located p)
located p = lexeme $ do
  start <- getOffset
  x <- p
  end <- getOffset
  return $ Located.located (Located.Region start end) x

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

scn :: Parser ()
scn = L.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

inParens :: Parser a -> Parser a
inParens = between (char '(') (char ')')

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = p `sepBy` lexeme (char ',')

oneOrCommaSeparatedInParens :: Parser a -> Parser [a]
oneOrCommaSeparatedInParens p = try (inParens (p `sepBy` lexeme (char ','))) <|> singleton <$> p