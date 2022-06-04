{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parse.Primitives where

import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Data.List
import Data.Text (Text, pack)
import Data.Void
import Elara.Name (Name (..))
import GHC.Read qualified as L
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Debug.Trace (traceShowM, traceM)

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

scn = L.space space1 lineComment empty

lexeme = L.lexeme sc

inParens :: Parser a -> Parser a
inParens = between (char '(') (char ')')

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = p `sepBy` (lexeme $ char ',')

oneOrCommaSeparatedInParens :: Parser a -> Parser [a]
oneOrCommaSeparatedInParens p = try (inParens (p `sepBy` (lexeme $ char ','))) <|> singleton <$> p


