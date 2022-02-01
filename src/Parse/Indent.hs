module Parse.Indent where

import Text.Parsec as Parsec
import qualified Text.Parsec.Indent as Indent

data LetExpr = LetExpr String [String] deriving (Show)

type IndentParser a = Indent.IndentParser String () a

bodyLine :: IndentParser String
bodyLine = Parsec.many1 Parsec.lower <* Parsec.spaces

letExpr :: IndentParser LetExpr
letExpr = Indent.withPos (LetExpr <$> bodyLine <*> Parsec.many (Indent.indented *> bodyLine))
