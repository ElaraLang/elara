module Parse.Pattern where

import AST.Source
import AST.Source qualified as SRC
import Data.Functor (($>), (<&>))
import Parse.Name
import Parse.Primitives (Parser, lexeme)
import Parse.Value qualified as V
import Text.Megaparsec (MonadParsec (try), choice, manyTill, sepBy)
import Text.Megaparsec.Char (char, string)

pattern :: Parser Pattern
pattern = choice [try consPattern, wildcard, varPattern, unitPattern, listPattern, constructorPattern, charPattern, stringPattern, intPattern, floatPattern]

wildcard :: Parser Pattern
wildcard = lexeme (char '_') $> SRC.PWildcard

varPattern :: Parser Pattern
varPattern = varName <&> SRC.PVar

unitPattern :: Parser Pattern
unitPattern = lexeme (string "()") $> SRC.PUnit

listPattern :: Parser Pattern
listPattern =
  SRC.PList <$> (lexeme (char '[') *> pattern `sepBy` lexeme (char ',')) <* lexeme (char ']')

bindPattern :: Parser Pattern
bindPattern =
  SRC.PBind <$> (varName <* lexeme (char '@')) <*> pattern

consPattern :: Parser Pattern
consPattern = do
  _ <- char '('
  p1 <- pattern
  _ <- char ':'
  p2 <- pattern
  _ <- char ')'
  return $ SRC.PCons p1 p2

constructorPattern :: Parser Pattern
constructorPattern = do
  _ <- lexeme (char '(')
  name <- typeName
  args <- manyTill pattern (lexeme (char ')'))
  return $ SRC.PConstructor name args

charPattern :: Parser Pattern
charPattern = PChar <$> V.rawCharacterLiteral

stringPattern :: Parser Pattern
stringPattern = PString <$> V.rawStringLiteral

intPattern :: Parser Pattern
intPattern = PInt <$> V.rawIntegerLiteral

floatPattern :: Parser Pattern
floatPattern = PFloat <$> V.rawFloatLiteral
