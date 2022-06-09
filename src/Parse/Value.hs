module Parse.Value where

import AST.Source qualified as SRC
import Data.Text qualified as T
import Parse.Primitives (Parser, lexeme)
import Text.Megaparsec (manyTill)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Megaparsec.Char.Lexer qualified as L

rawCharacterLiteral :: Parser Char
rawCharacterLiteral = lexeme (char '\'' *> charLiteral <* char '\'')

character :: Parser SRC.Expr
character = SRC.Char <$> rawCharacterLiteral

rawStringLiteral :: Parser T.Text
rawStringLiteral = T.pack <$> lexeme (char '"' >> manyTill charLiteral (char '"'))

string :: Parser SRC.Expr
string = SRC.String <$> rawStringLiteral

rawIntegerLiteral :: Parser Int
rawIntegerLiteral = lexeme L.decimal

integer :: Parser SRC.Expr
integer = SRC.Int <$> rawIntegerLiteral

rawFloatLiteral :: Parser Float
rawFloatLiteral = lexeme L.float

float :: Parser SRC.Expr
float = SRC.Float <$> rawFloatLiteral
