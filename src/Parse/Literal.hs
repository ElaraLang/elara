module Parse.Literal where

import AST.Source
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P

string :: P.Parser Expr
string = do
  _ <- P.char '"'
  body <- P.many (P.noneOf "\"")
  _ <- P.char '"'
  return $ StringE body

char :: P.Parser Expr
char = do
  _ <- P.char '\''
  content <- P.anyChar
  _ <- P.char '\''
  return $ CharE content

number :: P.Parser Expr
number = do
  integer <- P.many1 P.digit
  float <- P.optionMaybe $ P.char '.' >> P.many1 P.digit
  let intPart = read integer :: Integer
  return $ case float of
    Nothing -> IntE intPart
    Just floatPart -> FloatE $ fromIntegral intPart + read ("0." ++ floatPart)

literal :: P.Parser Expr
literal = string <|> char <|> number
