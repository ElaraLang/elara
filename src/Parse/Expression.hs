module Parse.Expression where

import AST.Source
import AST.Source qualified as Src
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import Data.Functor
import Data.Text qualified as T
import Parse.Name (opName, varName)
import Parse.Pattern (pattern)
import Parse.Primitives (Parser, commaSeparated, inParens, lexeme, sc)
import Parse.Value
import Text.Megaparsec (MonadParsec (try), choice, many, manyTill, noneOf, sepBy, some, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import Text.Megaparsec.Char.Lexer qualified as L

expr :: Parser Src.Expr
expr = makeExprParser term [[InfixL (Src.FunctionCall <$ sc)], [Prefix (Src.Negate <$ char '-')], [InfixL (Src.BinOp . Src.Var <$> opName)]]

term :: Parser Src.Expr
term = choice [inParens expr, try letInExpression, letExpression, character, string, try float, integer, variable, list, op, lambda]

variable :: Parser Src.Expr
variable = Src.Var <$> varName

list :: Parser Src.Expr
list = Src.List <$> (char '[' *> commaSeparated expr <* char ']')

op :: Parser Src.Expr
op = Src.Op <$> (inParens opName)

lambda :: Parser Src.Expr
lambda = do
  lexeme (char '\\')
  args <- lexeme (sepBy pattern sc)
  lexeme (C.string "->")
  Src.Lambda args <$> expr

def :: Parser (Src.Expr -> Src.Def)
def = choice [define, destruct]

define :: Parser (Src.Expr -> Src.Def)
define = do
  n <- lexeme varName
  args <- many pattern
  return $ Src.Define n args

destruct :: Parser (Src.Expr -> Src.Def)
destruct = Src.Destruct <$> pattern

letExpression :: Parser Src.Expr
letExpression = do
  lexeme (C.string "let")
  def <- lexeme def
  lexeme (char '=')
  exp <- expr
  return $ Src.Let (def exp) exp

letInExpression :: Parser Src.Expr
letInExpression = do
  lexeme (C.string "let")
  def <- lexeme def
  lexeme (char '=')
  val <- expr
  lexeme (C.string "in")
  exp <- expr
  return $ Src.LetIn (def exp) val exp
