module Parse.Language where

import Data.Functor.Identity (Identity)
import Parse.Indent (IndentParser, Parser)
import Text.Parsec (ParsecT, oneOf, (<|>))
import Text.Parsec.Char (alphaNum, char, letter)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token
import qualified Text.Parsec.Token as P
import Text.Parsec.Pos (SourcePos)
import Control.Monad.State

elaraLanguageDef :: GenLanguageDef String st (State SourcePos)
elaraLanguageDef =
  emptyDef
    { commentStart = "{-",
      commentEnd = "-}",
      commentLine = "--",
      nestedComments = True,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> oneOf "_'",
      opStart = operatorSymbol,
      opLetter = operatorSymbol,
      reservedOpNames = [],
      reservedNames = ["let"],
      caseSensitive = True
    }

elaraLexer :: GenTokenParser String () (State SourcePos)
elaraLexer = makeTokenParser elaraLanguageDef

identifier :: Parser String
identifier = P.identifier elaraLexer

parens :: Parser a -> Parser a
parens = P.parens elaraLexer

braces = P.braces elaraLexer

reserved = P.reserved elaraLexer

operatorSymbol :: ParsecT String u Identity Char -- WTF DOES THIS MEAN
operatorSymbol = oneOf "!#$%+-/*.<>=?@~\\^|"
