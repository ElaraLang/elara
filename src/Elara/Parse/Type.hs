module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Data.Text (Text)
import Elara.Data.Qualifications
import Elara.Data.Type
import Elara.Parse.Name (alphaVarName, moduleName)
import Elara.Parse.Primitives (Parser, lexeme, symbol)
import Text.Megaparsec (MonadParsec (try), choice, optional)
import Text.Megaparsec.Char qualified as C (string)
import Prelude hiding (bool)

type' :: Parser (ConcreteType MaybeQualified)
type' = makeExprParser typeTerm [[InfixR functionType]]

functionType :: Parser (ConcreteType MaybeQualified -> ConcreteType MaybeQualified -> ConcreteType MaybeQualified)
functionType = lexeme $ do
  symbol "->"
  return (\a b -> Concrete (Function a b) Nothing)

typeTerm :: Parser (ConcreteType MaybeQualified)
typeTerm =
  choice
    ( try . lexeme
        <$> [ typeVar,
              int,
              float,
              bool,
              char,
              string,
              unit
            ] ::
        [Parser (ConcreteType MaybeQualified)]
    )

typeVar :: Parser (ConcreteType MaybeQualified)
typeVar = maybeQualified $ Concrete . TypeVar <$> alphaVarName

int :: Parser (ConcreteType MaybeQualified)
int = Int <-> "Int"

float :: Parser (ConcreteType MaybeQualified)
float = Float <-> "Float"

bool :: Parser (ConcreteType MaybeQualified)
bool = Bool <-> "Bool"

char :: Parser (ConcreteType MaybeQualified)
char = Char <-> "Char"

string :: Parser (ConcreteType MaybeQualified)
string = String <-> "String"

unit :: Parser (ConcreteType MaybeQualified)
unit = Unit <-> "()"

(<->) :: AbsType Concrete MaybeQualified -> Text -> Parser (ConcreteType MaybeQualified)
(<->) t str = maybeQualified $ do
  symbol str
  return (Concrete t)

-- maybeQualified :: Parser (qual -> ConcreteType qual) -> Parser (ConcreteType qual)
maybeQualified :: Parser (MaybeQualified -> ConcreteType MaybeQualified) -> Parser (ConcreteType MaybeQualified)
maybeQualified p = do
  mod <- optional $ try (moduleName <* symbol ".")
  t <- p
  return (t mod)