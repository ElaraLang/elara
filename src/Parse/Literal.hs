module Parse.Literal where
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified Text.ParserCombinators.Parsec as P
import AST.Source

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

literal :: P.Parser Expr
literal = string <|> char 

