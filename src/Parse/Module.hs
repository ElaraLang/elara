module Parse.Module where

import AST.Source qualified as Src
import Data.Maybe (fromMaybe)
import Elara.Name (Name)
import Parse.Declaration
import Parse.Name
import Parse.Primitives (Parser, inParens, lexeme, oneOrCommaSeparatedInParens)
import Text.Megaparsec (choice, endBy, many, optional, parseMaybe, sepBy, sepEndBy, try)
import Text.Megaparsec.Char (char, newline, string)

module' :: Parser Src.Module
module' = do
  header <- parseHeader
  many newline
  imports <- many import'
  many newline
  decls <- declaration `sepEndBy` (many newline)
  return $ Src.Module (fst <$> header) (fromMaybe Src.Everything $ snd <$> header) imports (toValue <$> decls)

parseHeader :: Parser (Maybe (Name, Src.Exposing))
parseHeader = optional . try $ do
  -- module Name exposing (..)
  m <- lexeme (string "module")
  moduleName <- lexeme moduleName
  exposing <- exposing
  pure (moduleName, exposing)

exposing :: Parser Src.Exposing
exposing =
  fromMaybe Src.Everything
    <$> ( optional . try $ do
            lexeme (string "exposing")
            es <- lexeme (oneOrCommaSeparatedInParens exposingName)
            pure $ Src.Some es
        )

exposingName :: Parser Name
exposingName = choice [varName, typeName, inParens opName]

import' :: Parser Src.Import
import' = do
  i <- lexeme (string "import")
  name <- lexeme moduleName
  as <- optional . try $ do
    lexeme (string "as")
    lexeme typeName
  exposing <- exposing
  return $ Src.Import name as exposing