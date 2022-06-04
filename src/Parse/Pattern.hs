module Parse.Pattern where

import AST.Source
import AST.Source (Pattern)
import AST.Source qualified as SRC
import Control.Monad.Combinators.Expr
  ( Operator (InfixL, Prefix),
    makeExprParser,
  )
import Data.ByteString (cons)
import Data.Functor (($>), (<&>))
import Data.Text (pack)
import Data.Text qualified as T
import Parse.Name
import Parse.Primitives (Parser, lexeme, sc)
import Parse.Value qualified as V
import Text.Megaparsec (MonadParsec (try), choice, manyTill, noneOf, sepBy, (<?>), (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)
import Text.Megaparsec.Char.Lexer qualified as L

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
  char '('
  p1 <- pattern
  char ':'
  p2 <- pattern
  char ')'
  return $ SRC.PCons p1 p2

constructorPattern :: Parser Pattern
constructorPattern = do
  lexeme (char '(')
  name <- typeName
  args <- manyTill pattern (lexeme (char ')'))
  return $ SRC.PConstructor name args

charPattern :: Parser Pattern
charPattern = (\x -> let (Char c) = x in PChar c) <$> V.character

stringPattern :: Parser Pattern
stringPattern = (\x -> let (String c) = x in PString c) <$> V.string

intPattern :: Parser Pattern
intPattern = (\x -> let (Int c) = x in PInt c) <$> V.integer

floatPattern :: Parser Pattern
floatPattern = (\x -> let (Float c) = x in PFloat c) <$> V.float
