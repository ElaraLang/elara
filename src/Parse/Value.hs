module Parse.Value where

import qualified AST.Source as SRC
import qualified Data.Text as T
import Parse.Primitives (Parser, lexeme, opName, sc, varName)
import Text.Megaparsec (MonadParsec (try), choice, manyTill, noneOf, sepBy, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import qualified Text.Megaparsec.Char.Lexer as L

character :: Parser SRC.Expr
character = SRC.Char <$> lexeme (char '\'' *> charLiteral <* char '\'')

string :: Parser SRC.Expr
string = SRC.String . T.pack <$> lexeme (char '"' >> manyTill charLiteral (char '"'))

integer :: Parser SRC.Expr
integer = SRC.Int <$> lexeme L.decimal

float :: Parser SRC.Expr
float = SRC.Float <$> lexeme L.float