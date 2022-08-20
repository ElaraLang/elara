module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Data.Text (Text)
import Elara.Data.Qualifications
import Elara.Data.Type
import Elara.Parse.Name (alphaVarName, moduleName, typeName)
import Elara.Parse.Primitives (Parser, lexeme, sc, symbol)
import Text.Megaparsec (MonadParsec (try), choice, optional)
import Text.Megaparsec.Char qualified as C (string)
import Prelude hiding (bool)

type' :: Parser (ConcreteType MaybeQualified)
type' =
  makeExprParser
    typeTerm
    [ [InfixR constructorApplication],
      [InfixR functionType]
    ]

constructorApplication :: Parser (ConcreteType MaybeQualified -> ConcreteType MaybeQualified -> ConcreteType MaybeQualified)
constructorApplication = lexeme $ do
  sc
  return $ \t1 t2 -> Concrete (TypeConstructorApplication t1 t2) Nothing

functionType :: Parser (ConcreteType MaybeQualified -> ConcreteType MaybeQualified -> ConcreteType MaybeQualified)
functionType = lexeme $ do
  symbol "->"
  return (\a b -> Concrete (Function a b) Nothing)

typeTerm :: Parser (ConcreteType MaybeQualified)
typeTerm =
  choice
    ( try . lexeme
        <$> [ typeVar,
              unit,
              namedType
            ] ::
        [Parser (ConcreteType MaybeQualified)]
    )

typeVar :: Parser (ConcreteType MaybeQualified)
typeVar = maybeQualified $ Concrete . TypeVar <$> alphaVarName

unit :: Parser (ConcreteType MaybeQualified)
unit = Unit <-> "()"

namedType :: Parser (ConcreteType MaybeQualified)
namedType = maybeQualified $ do
  name <- typeName
  return $ \qual -> Concrete (UserDefinedType qual name) qual

(<->) :: AbsType Concrete MaybeQualified -> Text -> Parser (ConcreteType MaybeQualified)
(<->) t str = maybeQualified $ do
  symbol str
  return (Concrete t)

maybeQualified :: Parser (MaybeQualified -> ConcreteType MaybeQualified) -> Parser (ConcreteType MaybeQualified)
maybeQualified p = do
  mod <- optional $ try (moduleName <* symbol ".")
  t <- p
  return (t mod)