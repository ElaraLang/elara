module Parse.Declaration where

import AST.Source (Expr (BlockExpr))
import AST.Source qualified as Src
import Control.Monad (void)
import Data.Text qualified as T
import Elara.Name
import Parse.Expression (expr)
import Parse.Name
import Parse.Primitives
import Text.Megaparsec
  ( MonadParsec (try),
    choice,
    many,
    mkPos,
    oneOf,
    some,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (lexeme)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Parser.Combinators hiding (many, some, try)

newtype Decl
  = Value Src.Value

instance Show Decl where
  show (Value v) = show v

toValue :: Decl -> Src.Value
toValue (Value v) = v

declaration :: Parser Decl
declaration = valueDecl

valueDecl :: Parser Decl
valueDecl = do
  (name, e) <- optionallyIndented letPreamble
  return $ Value $ Src.Value name [] e Nothing
  where
    letPreamble = do
      lexeme (string "let")
      name <- varName
      lexeme (char '=')
      return name

optionallyIndented :: Parser a -> Parser (a, Expr)
optionallyIndented a = try nonIndented <|> try (indentedBlock a)
  where
    nonIndented = do
      a <- a
      b <- expr
      return (a, b)

indentedBlock :: Parser a -> Parser (a, Expr)
indentedBlock ref = L.nonIndented scn $
  L.indentBlock scn $ do
    a <- ref
    return $ L.IndentSome Nothing (\x -> return (a, BlockExpr x)) expr
