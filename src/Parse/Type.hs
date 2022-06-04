module Parse.Type where

import AST.Source
import qualified AST.Source as SRC
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import Data.Functor
import qualified Data.Text as T
import Parse.Pattern (pattern)
import Parse.Primitives (Parser, lexeme, sc)
import Parse.Value
import Text.Megaparsec (MonadParsec (try), choice, many, manyTill, noneOf, sepBy, some, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import qualified Text.Megaparsec.Char.Lexer as L

