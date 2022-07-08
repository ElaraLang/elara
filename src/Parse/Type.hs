module Parse.Type (type') where

import AST.Source qualified as Src
import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Parse.Name (alphaVarName, typeName)
import Parse.Primitives (Parser, inParens, lexeme)
import Text.Megaparsec (MonadParsec (try), choice)
import Text.Megaparsec.Char (string)

type' :: Parser Src.Type
type' = makeExprParser typeTerm [[InfixR (Src.TLambda <$ lexeme (string "->"))]]

typeTerm :: Parser Src.Type
typeTerm = choice [try concreteType, try typeVariable, try bracketedType]

concreteType :: Parser Src.Type
concreteType = Src.TCon <$> lexeme typeName

typeVariable :: Parser Src.Type
typeVariable = Src.TVar <$> lexeme alphaVarName

bracketedType :: Parser Src.Type
bracketedType = lexeme $ inParens type'