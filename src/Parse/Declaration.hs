module Parse.Declaration where

import AST.Source (Expr (BlockExpr))
import qualified AST.Source as SRC
import qualified Data.Text as T
import Elara.Name
import Parse.Expression (expr)
import Parse.Primitives
import Text.Megaparsec (many, mkPos, oneOf, some, (<?>))
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (lexeme)
import qualified Text.Megaparsec.Char.Lexer as L

newtype Decl
  = Value SRC.Value
  deriving (Show)

declaration :: Parser Decl
declaration = valueDecl

valueDecl :: Parser Decl
valueDecl = L.indentBlock scn $ do
  lexeme (string "let")
  name <- varName
  lexeme (string "=")
  return $ L.IndentSome Nothing (\e -> return $ Value $ SRC.Value name [] (BlockExpr e) Nothing) expr