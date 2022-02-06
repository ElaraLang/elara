module Parse.Literal where

import AST.Source
import Parse.Indent (IndentParser)
import Text.Parsec (spaces, try)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

string :: IndentParser Expr
string = try $ do
  _ <- P.char '"'
  body <- P.many (P.noneOf "\"")
  _ <- P.char '"'
  return $ StringE body

char :: IndentParser Expr
char = try $ do
  _ <- P.char '\''
  content <- P.anyChar
  _ <- P.char '\''
  return $ CharE content

number :: IndentParser Expr
number = try $ do
  integer <- P.many1 P.digit
  float <- P.optionMaybe $ try (P.char '.' *> P.many1 P.digit)
  let intPart = read integer
  return $ case float of
    Nothing -> IntE intPart
    Just floatPart -> FloatE $ fromIntegral intPart + read ("0." ++ floatPart)

literal :: IndentParser Expr
literal = try (string <|> char <|> number) <* spaces
