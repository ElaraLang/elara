module Parse.Construct where

import AST.Source
import Parse.Indent (IndentParser)
import Parse.Language
import Parse.Literal
import Text.Parsec (spaces, try, (<?>))
import qualified Text.Parsec as P
import Text.Parsec.Expr (Assoc (..), Operator (..), buildExpressionParser)
import qualified Text.Parsec.Indent as Indent
import Text.Parsec.Token (reservedOp)
import Text.ParserCombinators.Parsec ((<|>))

letter :: [Char]
letter = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']

letterOrDigit :: [Char]
letterOrDigit = letter ++ ['0' .. '9']

normalIdentifier :: IndentParser Identifier
normalIdentifier = do
  c <- P.oneOf letter
  cs <- P.many (P.oneOf letterOrDigit)
  return $ NormalIdentifier (c : cs)

type_ :: IndentParser Type
type_ =
  (P.string "()" >> return UnitType)
    <|> (TypeVariable <$> normalIdentifier)

def :: IndentParser Def
def = do
  _ <- P.string "def"
  spaces
  name <- identifier
  spaces
  _ <- P.char ':'
  spaces
  Def name <$> type_

letName :: IndentParser Identifier
letName = do
  _ <- P.string "let"
  spaces
  s <- normalIdentifier
  spaces
  _ <- P.char '='
  spaces
  return s

let_ :: IndentParser Let
let_ = Indent.withPos $ do
  def_ <- P.optionMaybe (def <* P.newline)
  b <- Indent.withBlock ((. Block) . Let def_) letName (line <* spaces)
  spaces
  return b

line :: IndentParser Line
line = linePart
  where
    linePart =
      DefLetLine <$> let_
        <|> ExprLine <$> expression

operatorTable =
  [ [],
    [],
    [standardBinary "*" AssocLeft, standardBinary "/" AssocLeft],
    [standardBinary "+" AssocLeft, standardBinary "-" AssocLeft]
  ]

standardBinary :: String -> Assoc -> Operator s2 u2 m2 Expr
standardBinary c = binary c (BinaryOp c)

binary :: String -> (a -> a -> a) -> Assoc -> Operator s2 u2 m2 a
binary name fun = Infix $ do
  reservedOp elaraLexer name
  return fun

parseOp :: IndentParser (Expr -> Expr -> Expr)
parseOp = do
  spaces
  symbol <- P.many1 operatorSymbol
  spaces
  return $ BinaryOp symbol

namedReference :: IndentParser Expr
namedReference = try (spaces *> (NamedReference <$> identifier) <* spaces)

term :: IndentParser Expr
term = literal <|> namedReference

expression :: IndentParser Expr
expression = buildExpressionParser operatorTable term

file :: IndentParser [Line]
file = P.manyTill line (try P.eof)
