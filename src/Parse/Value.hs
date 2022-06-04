module Parse.Value where

import AST.Source qualified as SRC
import Data.Text qualified as T
import Parse.Primitives (Parser, lexeme, sc)
import Text.Megaparsec (MonadParsec (try), choice, manyTill, noneOf, sepBy, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import Text.Megaparsec.Char.Lexer qualified as L

character :: Parser SRC.Expr
character = SRC.Char <$> lexeme (char '\'' *> charLiteral <* char '\'')

string :: Parser SRC.Expr
string = SRC.String . T.pack <$> lexeme (char '"' >> manyTill charLiteral (char '"'))

integer :: Parser SRC.Expr
integer = SRC.Int <$> lexeme L.decimal

float :: Parser SRC.Expr
float = SRC.Float <$> lexeme L.float