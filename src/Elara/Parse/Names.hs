module Elara.Parse.Names where

import Elara.AST.Name (MaybeQualified (..), ModuleName (..), OpName (..), TypeName (..), VarName (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Primitives (Parser, inParens, lexeme)
import Text.Megaparsec (MonadParsec (try), oneOf, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)

varName :: Parser (MaybeQualified VarName)
varName = try operatorVarName <|> normalVarName

normalVarName :: Parser (MaybeQualified VarName)
normalVarName = maybeQualified (NormalVarName <$> alphaVarName) <?> "variable name"

operatorVarName :: Parser (MaybeQualified VarName)
operatorVarName = (OperatorVarName <<$>> inParens opName) <?> "operator name in parens"

typeName :: Parser (MaybeQualified TypeName)
typeName = do
  ModuleName names <- moduleName
  pure $ case names of
    x :| [] -> MaybeQualified (TypeName x) Nothing
    _ -> MaybeQualified (TypeName (last names)) (Just $ ModuleName (fromList $ init names))

maybeQualified :: Parser name -> Parser (MaybeQualified name)
maybeQualified name = do
  qual <- optional . try $ (moduleName <* char '.')
  name' <- name
  pure $ MaybeQualified name' qual

moduleName :: Parser ModuleName
moduleName = ModuleName <$> sepBy1' upperVarName (char '.')

upperVarName :: Parser Text
upperVarName = toText <$> ((:) <$> upperChar <*> many alphaNumChar)

alphaVarName :: Parser Text
alphaVarName = toText <$> ((:) <$> lowerChar <*> many alphaNumChar)

opName :: Parser (MaybeQualified OpName)
opName = maybeQualified $ OpName . toText <$> lexeme (some operatorChar)
 where
  operatorChars :: [Char]
  operatorChars = ['!', '#', '$', '%', '&', '*', '+', '.', '/', '\\', '<', '>', '=', '?', '@', '^', '|', '-', '~']
  operatorChar :: Parser Char
  operatorChar = oneOf operatorChars