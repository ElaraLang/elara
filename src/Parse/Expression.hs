module Parse.Expression where

import AST.Source
import qualified AST.Source as SRC
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import Data.Functor
import qualified Data.Text as T
import Parse.Pattern (pattern)
import Parse.Primitives (Parser, lexeme, opName, sc, varName)
import Parse.Value
import Text.Megaparsec (MonadParsec (try), choice, many, manyTill, noneOf, sepBy, some, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import qualified Text.Megaparsec.Char.Lexer as L

expr :: Parser SRC.Expr
expr = makeExprParser term [[Prefix (SRC.Negate <$ char '-')], [InfixL (SRC.BinOp . SRC.Var <$> opName)]]

term = choice [try letInExpression, try letExpression, character, string, try float, integer, variable, list, op, lambda]

variable :: Parser SRC.Expr
variable = SRC.Var <$> varName

list :: Parser SRC.Expr
list = SRC.List <$> (char '[' *> sepBy expr (lexeme (char ',')) <* char ']')

op :: Parser SRC.Expr
op = SRC.Op <$> (char '(' *> opName <* char ')')

lambda :: Parser SRC.Expr
lambda = do
  lexeme (char '\\')
  args <- lexeme (sepBy pattern sc)
  lexeme (C.string "->")
  SRC.Lambda args <$> expr

def :: Parser SRC.Def
def = choice [define, destruct]

define :: Parser SRC.Def
define = do
  n <- lexeme varName
  args <- many pattern
  return $ SRC.Define n args

destruct :: Parser SRC.Def
destruct = pattern <&> SRC.Destruct

letExpression :: Parser SRC.Expr
letExpression = do
  lexeme (C.string "let")
  def <- lexeme def
  lexeme (char '=')
  SRC.Let def <$> expr

letInExpression :: Parser SRC.Expr
letInExpression = do
  lexeme (C.string "let")
  def <- lexeme def
  lexeme (char '=')
  val <- expr
  lexeme (C.string "in")
  SRC.LetIn def val <$> expr
