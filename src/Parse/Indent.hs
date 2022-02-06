module Parse.Indent where

import Text.Parsec as Parsec
import qualified Text.Parsec.Indent as Indent

data LetExpr = LetExpr String [String] deriving (Show)

type Parser = ParsecT String () (State SourcePos)

type IndentParser a = Indent.IndentParser String () a