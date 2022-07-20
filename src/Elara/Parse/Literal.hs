module Elara.Parse.Literal where

import Elara.Parse.Primitives (Parser, lexeme)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer as L (charLiteral, decimal, float)
import Text.Parser.Combinators (manyTill, surroundedBy)

charLiteral :: Parser Char
charLiteral = lexeme (surroundedBy (char '\'') (L.charLiteral))

stringLiteral :: Parser String
stringLiteral = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

integerLiteral :: Parser Int
integerLiteral = lexeme (L.decimal)

floatLiteral :: Parser Double
floatLiteral = lexeme (L.float)
