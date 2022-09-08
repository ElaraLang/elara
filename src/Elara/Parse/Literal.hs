module Elara.Parse.Literal where

import Elara.Parse.Primitives (Parser, lexeme, sc, symbol)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer as L (charLiteral, decimal, float, signed)
import Text.Parser.Combinators (manyTill, surroundedBy)

charLiteral :: Parser Char
charLiteral = lexeme (surroundedBy L.charLiteral (symbol "'"))

stringLiteral :: Parser String
stringLiteral = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

integerLiteral :: Parser Int
integerLiteral = L.signed sc (lexeme L.decimal)

floatLiteral :: Parser Double
floatLiteral = L.signed sc (lexeme L.float)