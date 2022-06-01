module Parse.Expression where

import AST.Source
import qualified AST.Source as SRC
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import qualified Data.Text as T
import Parse.Pattern (pattern)
import Parse.Primitives (Parser, lexeme, opName, sc, varName)
import Parse.Value
import Text.Megaparsec (MonadParsec (try), choice, manyTill, noneOf, sepBy, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import qualified Text.Megaparsec.Char.Lexer as L

expr :: Parser SRC.Expr
expr = makeExprParser term [[Prefix (SRC.Negate <$ char '-')], [InfixL (SRC.BinOp . SRC.Var <$> opName)]]

term = choice [character, string, try float, integer, variable, list, op, lambda]

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