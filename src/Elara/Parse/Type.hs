module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Data.Text (Text)
import Elara.Data.Type
import Elara.Parse.Name (alphaVarName)
import Elara.Parse.Primitives (Parser, lexeme)
import Text.Megaparsec (MonadParsec (try), choice)
import Text.Megaparsec.Char qualified as C (string)

type' :: Parser (ConcreteType qual)
type' = makeExprParser typeTerm [[InfixR functionType]]

functionType :: Parser (ConcreteType qual -> ConcreteType qual -> ConcreteType qual)
functionType = do
  _ <- lexeme (C.string "->")
  return ((makeConcrete .) . Function)

typeTerm :: Parser (ConcreteType qual)
typeTerm =
  makeConcrete
    <$> choice
      ( try . lexeme
          <$> [ typeVar,
                int,
                float,
                bool,
                char,
                string,
                unit
              ]
      )

typeVar :: Parser (ConcreteAbs qual)
typeVar = TypeVar <$> alphaVarName

int :: Parser (ConcreteAbs qual)
int = Int <-> "Int"

float :: Parser (ConcreteAbs qual)
float = Float <-> "Float"

bool :: Parser (ConcreteAbs qual)
bool = Bool <-> "Bool"

char :: Parser (ConcreteAbs qual)
char = Char <-> "Char"

string :: Parser (ConcreteAbs qual)
string = String <-> "String"

unit :: Parser (ConcreteAbs qual)
unit = Unit <-> "()"

(<->) :: ConcreteAbs qual -> Text -> Parser (ConcreteAbs qual)
(<->) t str = t <$ C.string str