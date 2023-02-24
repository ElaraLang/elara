module Elara.Parse.Names where

import Data.Set (member)
import Elara.AST.Name (MaybeQualified (..), ModuleName (..), OpName (..), TypeName (..), VarName (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Primitives (HParser, inParens, lexeme, symbol, (<??>))
import HeadedMegaparsec (endHead)
import HeadedMegaparsec qualified as H (parse)
import Text.Megaparsec (satisfy)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, upperChar)

varName :: HParser (MaybeQualified VarName)
varName = operatorVarName <|> normalVarName

normalVarName :: HParser (MaybeQualified VarName)
normalVarName = maybeQualified (NormalVarName <$> alphaVarName) <??> "variable name"

operatorVarName :: HParser (MaybeQualified VarName)
operatorVarName = (OperatorVarName <<$>> inParens opName) <??> "operator name in parens"

typeName :: HParser (MaybeQualified TypeName)
typeName = do
  ModuleName names <- moduleName
  pure $ case names of
    x :| [] -> MaybeQualified (TypeName x) Nothing
    _ -> MaybeQualified (TypeName (last names)) (Just $ ModuleName (fromList $ init names))

maybeQualified :: HParser name -> HParser (MaybeQualified name)
maybeQualified name = unqualified <|> qualified
 where
  unqualified = MaybeQualified <$> name <*> pure Nothing
  qualified = do
    qual <- moduleName
    endHead
    symbol "."
    MaybeQualified <$> name <*> pure (Just qual)

moduleName :: HParser ModuleName
moduleName = ModuleName <$> sepBy1' upperVarName (H.parse $ char '.')

upperVarName :: HParser Text
upperVarName =
  toText <$> do
    start <- H.parse upperChar
    rest <- H.parse (many alphaNumChar)
    pure (start : rest)

alphaVarName :: HParser Text
alphaVarName =
  toText <$> do
    start <- H.parse lowerChar
    rest <- H.parse (many alphaNumChar)
    pure (start : rest)

opName :: HParser (MaybeQualified OpName)
opName = maybeQualified $ OpName . toText <$> lexeme (some operatorChar)
 where
  operatorChars :: Set Char
  operatorChars = ['!', '#', '$', '%', '&', '*', '+', '.', '/', '\\', '<', '>', '=', '?', '@', '^', '|', '-', '~']
  operatorChar :: HParser Char
  operatorChar = H.parse $ satisfy (`member` operatorChars)