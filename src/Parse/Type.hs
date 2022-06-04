module Parse.Type where

import AST.Source
import AST.Source qualified as SRC
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import Data.Functor
import Data.Text qualified as T
import Parse.Pattern (pattern)
import Parse.Primitives (Parser, lexeme, sc)
import Parse.Value
import Text.Megaparsec (MonadParsec (try), choice, many, manyTill, noneOf, sepBy, some, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import Text.Megaparsec.Char.Lexer qualified as L
