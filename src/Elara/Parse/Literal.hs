module Elara.Parse.Literal (charLiteral, stringLiteral, integerLiteral, floatLiteral) where

import Elara.Parse.Primitives (HParser, IsParser (fromParsec), Parser, char, char', lexeme)
import HeadedMegaparsec qualified as H
import Text.Megaparsec (manyTill)
import Text.Megaparsec.Char.Lexer qualified as L (charLiteral, decimal, float, signed)

charLiteral :: HParser Char
charLiteral = lexeme $ do
  char' '\''
  H.endHead
  c <- H.parse L.charLiteral
  char' '\''
  pure c

stringLiteral :: HParser Text
stringLiteral = lexeme $ do
  char' '"'
  H.endHead
  toText <$> manyTill (fromParsec L.charLiteral) (char '"')

{- | Parses a signed integer literal
  Note that this parser does not allow whitespace between the sign and the number.
  This is because it would be ambiguous with an infix operator like `+` if this was allowed
-}
signed :: (Num a) => Parser a -> HParser a
signed p = fromParsec $ L.signed pass p

integerLiteral :: HParser Integer
integerLiteral = signed (lexeme L.decimal)

floatLiteral :: HParser Double
floatLiteral = signed (lexeme L.float)