module Parse.Declaration where

import AST.Source (Expr (BlockExpr))
import AST.Source qualified as Src
import Parse.Expression (expr)
import Parse.Name
import Parse.Pattern (pattern)
import Parse.Primitives
import Text.Megaparsec
  ( MonadParsec (try),
    many,
    (<|>),
  )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

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
  ((name, patterns), e) <- optionallyIndented letPreamble
  return $ Value $ Src.Value name patterns e Nothing
  where
    letPreamble = do
      _ <- lexeme (string "let")
      name <- varName
      patterns <- many pattern
      _ <- lexeme (char '=')
      return (name, patterns)

optionallyIndented :: Parser a -> Parser (a, Expr)
optionallyIndented a = try nonIndented <|> try (indentedBlock a)
  where
    nonIndented = do
      a' <- a
      b <- expr
      return (a', b)

indentedBlock :: Parser a -> Parser (a, Expr)
indentedBlock ref = L.nonIndented scn $
  L.indentBlock scn $ do
    a <- ref
    return $ L.IndentSome Nothing (\x -> return (a, BlockExpr x)) expr
