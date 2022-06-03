module Parse.Expression where

import AST.Source
import AST.Source qualified as SRC
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import Data.Functor
import Data.Text qualified as T
import Parse.Pattern (pattern)
import Parse.Primitives (Parser, lexeme, opName, sc, varName, inParens, commaSeparated)
import Parse.Value
import Text.Megaparsec (MonadParsec (try), choice, many, manyTill, noneOf, sepBy, some, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import Text.Megaparsec.Char.Lexer qualified as L

expr :: Parser SRC.Expr
expr = makeExprParser term [[Prefix (SRC.Negate <$ char '-')], [InfixL (SRC.BinOp . SRC.Var <$> opName)]]

term :: Parser SRC.Expr
term = choice [try letInExpression, letExpression, character, string, try float, integer, variable, list, op, lambda]

variable :: Parser SRC.Expr
variable = SRC.Var <$> varName

list :: Parser SRC.Expr
list = SRC.List <$> (char '[' *> commaSeparated expr <* char ']')

op :: Parser SRC.Expr
op = SRC.Op <$> (inParens opName)

lambda :: Parser SRC.Expr
lambda = do
  lexeme (char '\\')
  args <- lexeme (sepBy pattern sc)
  lexeme (C.string "->")
  SRC.Lambda args <$> expr

def :: Parser (SRC.Expr -> SRC.Def)
def = choice [define, destruct]

define :: Parser (SRC.Expr -> SRC.Def)
define = do
  n <- lexeme varName
  args <- many pattern
  return $ SRC.Define n args

destruct :: Parser (SRC.Expr -> SRC.Def)
destruct = SRC.Destruct <$> pattern

letExpression :: Parser SRC.Expr
letExpression = do
  lexeme (C.string "let")
  def <- lexeme def
  lexeme (char '=')
  exp <- expr
  return $ SRC.Let (def exp) exp

letInExpression :: Parser SRC.Expr
letInExpression = do
  lexeme (C.string "let")
  def <- lexeme def
  lexeme (char '=')
  val <- expr
  lexeme (C.string "in")
  exp <- expr
  return $ SRC.LetIn (def exp) val exp
