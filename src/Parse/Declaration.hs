module Parse.Declaration where

import AST.Source (Expr (BlockExpr))
import AST.Source qualified as Src
import Elara.Name (Name)
import Parse.Expression (expr)
import Parse.Name
import Parse.Pattern (pattern)
import Parse.Primitives
import Parse.Type (type')
import Text.Megaparsec
  ( MonadParsec (try),
    many,
    (<|>),
  )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Decl
  = Value Src.Value
  | Def Src.Decl

instance Show Decl where
  show (Value v) = show v
  show (Def n) = show n

asAST :: Decl -> Either Src.Value Src.Decl
asAST (Value v) = Left v
asAST (Def n) = Right n

declaration :: Parser Decl
declaration = defDecl <|> valueDecl

defDecl :: Parser Decl
defDecl = do
  _ <- lexeme "def"
  name <- varName
  _ <- lexeme ":"
  Def . Src.Decl name <$> type'

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