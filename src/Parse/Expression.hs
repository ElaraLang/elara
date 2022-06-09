module Parse.Expression where

import AST.Source qualified as Src
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import Parse.Name (opName, varName)
import Parse.Pattern (pattern)
import Parse.Primitives (Parser, commaSeparated, inParens, lexeme, sc)
import Parse.Value
import Text.Megaparsec (MonadParsec (try), choice, many, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char qualified as C

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
  _ <- lexeme (char '\\')
  args <- lexeme (sepBy pattern sc)
  _ <- lexeme (C.string "->")
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
  _ <- lexeme (C.string "let")
  d <- lexeme def
  _ <- lexeme (char '=')
  e <- expr
  return $ Src.Let (d e) e

letInExpression :: Parser Src.Expr
letInExpression = do
  _ <- lexeme (C.string "let")
  d <- lexeme def
  _ <- lexeme (char '=')
  val <- expr
  _ <- lexeme (C.string "in")
  e <- expr
  return $ Src.LetIn (d e) val e
