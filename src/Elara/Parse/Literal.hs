module Elara.Parse.Literal (charLiteral, stringLiteral, integerLiteral, floatLiteral) where

import Elara.Parse.Primitives (Parser, lexeme)
import Text.Megaparsec (manyTill)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L (charLiteral, decimal, float, signed)

charLiteral :: Parser Char
charLiteral = lexeme ("'" *> L.charLiteral <* "'")

stringLiteral :: Parser Text
stringLiteral = toText <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

{- | Parses a signed integer literal
  Note that this parser does not allow whitespace between the sign and the number.
  This is because it would be ambiguous with an infix operator like `+` if this was allowed
-}
signed :: (Num a) => Parser a -> Parser a
signed = L.signed pass

integerLiteral :: Parser Integer
integerLiteral = signed (lexeme L.decimal)

floatLiteral :: Parser Double
floatLiteral = signed (lexeme L.float)