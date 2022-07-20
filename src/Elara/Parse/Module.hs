module Elara.Parse.Module where

import Data.Maybe (fromMaybe, isJust)
import Elara.AST.Frontend (LocatedExpr)
import Elara.Data.Module (Exposing (..), Exposition (ExposedValue), Import (..), Module (..), name)
import Elara.Data.Name (ModuleName)
import Elara.Data.Name qualified as Name
import Elara.Data.TypeAnnotation
import Elara.Parse.Declaration
import Elara.Parse.Name (moduleName, varName)
import Elara.Parse.Primitives (Parser, lexeme, oneOrCommaSeparatedInParens)
import Text.Megaparsec (MonadParsec (try), many, optional, sepEndBy)
import Text.Megaparsec.Char (newline, string)

module' :: Parser (Module LocatedExpr TypeAnnotation (Maybe ModuleName))
module' = do
  header <- parseHeader
  let _name = maybe (Name.fromString "Main") fst header
  _ <- many newline

  imports <- import' `sepEndBy` many newline
  declarations <- declaration _name `sepEndBy` many newline

  return $
    Module
      { _name = _name,
        _exposing = maybe ExposingAll snd header,
        _imports = imports,
        _declarations = declarations
      }

parseHeader :: Parser (Maybe (ModuleName, Exposing))
parseHeader = optional . try $ do
  -- module Name exposing (..)
  _ <- lexeme (string "module")
  moduleName' <- lexeme moduleName
  exposing' <- exposing
  pure (moduleName', exposing')

exposing :: Parser Exposing
exposing =
  fromMaybe ExposingAll
    <$> ( optional . try $ do
            _ <- lexeme (string "exposing")
            es <- lexeme (oneOrCommaSeparatedInParens exposition)
            pure $ ExposingSome es
        )

exposition :: Parser Exposition
exposition = ExposedValue <$> varName

import' :: Parser Import
import' = do
  _ <- lexeme (string "import")
  name <- lexeme moduleName
  qualified <- optional (lexeme (string "qualified"))
  as <- optional . try $ do
    _ <- lexeme (string "as")
    lexeme moduleName
  Import name as (isJust qualified) <$> exposing