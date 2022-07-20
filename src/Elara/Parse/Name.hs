module Elara.Parse.Name (varName, typeName, opName, moduleName, alphaVarName) where

import Data.Text qualified as T
import Elara.Data.Name (ModuleName, Name (QualifiedName), NameFromText)
import Elara.Data.Name qualified as Name
import Elara.Parse.Primitives (Parser, inParens, lexeme)
import Text.Megaparsec
import Text.Megaparsec.Char

varName :: Parser Name
varName = qualified varName'
  where
    varName' = lexeme (inParens opName) <|> lexeme alphaVarName

alphaVarName :: (NameFromText n) => Parser n
alphaVarName = Name.fromString <$> lexeme ((:) <$> lowerChar <*> many alphaNumChar)

typeName :: Parser Name
typeName = qualified (Name.fromString <$> capitalizedString)

capitalizedString :: Parser String
capitalizedString = lexeme $ do
  x <- upperChar
  xs <- many alphaNumChar
  return (x : xs)

opName :: Parser Name
opName = qualified opName'
  where
    opName' :: Parser Name
    opName' = Name.fromString <$> lexeme (some operatorChar)
    operatorChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)

moduleName :: Parser ModuleName
moduleName = do
  parts <- sepEndBy1 capitalizedString (char '.') :: Parser [String]
  return (Name.ModuleName (T.pack <$> parts))

qualified :: Parser Name -> Parser Name
qualified parser = do
  module' <- optional moduleName
  QualifiedName module' <$> parser