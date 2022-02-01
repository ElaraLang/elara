module Parse.Construct where

import AST.Source
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, (<|>))

operatorSymbol :: Parser Char
operatorSymbol = P.oneOf "!#$%+-/*.<>=?@~\\^|"

identifier :: Parser Identifier
identifier = do
  paren <- P.optionMaybe (P.char '(')
  case paren of
    Just _ -> do
      body <- P.many1 operatorSymbol
      _ <- P.char ')'
      return $ OpIdentifier body
    Nothing -> do
      body <- P.many1 P.anyChar -- TODO not just any char
      return $ NormalIdentifier body

let' :: Parser Expr
let' = do
  _ <- P.string "let"
  mut <- P.optional (P.string "mut")
  name <- identifier
  eq <- P.char '='
  error "a"
