module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Elara.AST.Name (MaybeQualified, ModuleName)
import Elara.Data.Type (Type (..))
import Elara.Parse.Names (alphaVarName, moduleName, typeName)
import Elara.Parse.Primitives (Parser, lexeme, sc, symbol)
import Text.Megaparsec (choice, try)
import Prelude hiding (Type)

type' :: Parser (Type MaybeQualified)
type' =
    makeExprParser
        typeTerm
        [ [InfixR constructorApplication]
        , [InfixR functionType]
        ]

constructorApplication :: Parser (Type MaybeQualified -> Type MaybeQualified -> Type MaybeQualified)
constructorApplication = lexeme (TypeConstructorApplication <$ sc)

functionType :: Parser (Type MaybeQualified -> Type MaybeQualified -> Type MaybeQualified)
functionType = lexeme (FunctionType <$ symbol "->")

typeTerm :: Parser (Type MaybeQualified)
typeTerm =
    choice
        ( try . lexeme
            <$> [ typeVar
                , unit
                , namedType
                ] ::
            [Parser (Type MaybeQualified)]
        )

typeVar :: Parser (Type MaybeQualified)
typeVar = TypeVar <$> alphaVarName

unit :: Parser (Type MaybeQualified)
unit = UnitType <$ symbol "()"

namedType :: Parser (Type MaybeQualified)
namedType = UserDefinedType <$> typeName

maybeQualified :: Parser (Maybe ModuleName -> b) -> Parser b
maybeQualified p = do
    moduleQualification <- optional $ try (moduleName <* symbol ".")
    t <- p
    pure (t moduleQualification)