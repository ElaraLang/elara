module Parse.Module where

import AST.Source qualified as Src
import Data.Maybe (fromMaybe)
import Elara.Name (Name)
import Parse.Declaration
import Parse.Name
import Parse.Primitives (Parser, inParens, lexeme, oneOrCommaSeparatedInParens)
import Text.Megaparsec (choice, many, optional, sepEndBy, try)
import Text.Megaparsec.Char (newline, string)

module' :: Parser Src.Module
module' = do
  header <- parseHeader
  _ <- many newline
  imports <- many import'
  _ <- many newline
  decls <- declaration `sepEndBy` (many newline)
  return $ Src.Module (fst <$> header) (fromMaybe Src.Everything $ snd <$> header) imports (toValue <$> decls)

parseHeader :: Parser (Maybe (Name, Src.Exposing))
parseHeader = optional . try $ do
  -- module Name exposing (..)
  _ <- lexeme (string "module")
  moduleName' <- lexeme moduleName
  exposing' <- exposing
  pure (moduleName', exposing')

exposing :: Parser Src.Exposing
exposing =
  fromMaybe Src.Everything
    <$> ( optional . try $ do
            _ <- lexeme (string "exposing")
            es <- lexeme (oneOrCommaSeparatedInParens exposingName)
            pure $ Src.Some es
        )

exposingName :: Parser Name
exposingName = choice [varName, typeName, inParens opName]

import' :: Parser Src.Import
import' = do
  _ <- lexeme (string "import")
  name <- lexeme moduleName
  as <- optional . try $ do
    _ <- lexeme (string "as")
    lexeme typeName
  exposing' <- exposing
  return $ Src.Import name as exposing'