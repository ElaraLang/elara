{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parse.Primitives where

import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Void
import Elara.Name (Name (..))
import qualified GHC.Read as L
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

scn = L.space space1 lineComment empty

lexeme = L.lexeme sc

varName :: Parser Name
varName = VarName . pack <$> lexeme ((:) <$> lowerChar <*> many alphaNumChar)

typeName :: Parser Name
typeName = TypeName . pack <$> lexeme ((:) <$> upperChar <*> many alphaNumChar)

opName :: Parser Name
opName = OpName . pack <$> lexeme (some operatorChar)
  where
    operatorChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)