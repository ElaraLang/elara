module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Elara.AST.Name (MaybeQualified, ModuleName)
import Elara.Data.Type (Type (..))
import Elara.Parse.Names (alphaVarName, moduleName, typeName)
import Elara.Parse.Primitives (HParser, lexeme, sc, symbol)
import Text.Megaparsec (choice)
import Prelude hiding (Type)

type' :: HParser (Type MaybeQualified)
type' =
    makeExprParser
        typeTerm
        [ [InfixR constructorApplication]
        , [InfixR functionType]
        ]

constructorApplication :: HParser (Type MaybeQualified -> Type MaybeQualified -> Type MaybeQualified)
constructorApplication = lexeme (TypeConstructorApplication <$ (sc :: HParser ()))

functionType :: HParser (Type MaybeQualified -> Type MaybeQualified -> Type MaybeQualified)
functionType = lexeme (FunctionType <$ (symbol "->" :: HParser ()))

typeTerm :: HParser (Type MaybeQualified)
typeTerm =
    choice
        ( lexeme
            <$> [ typeVar
                , unit
                , namedType
                ] ::
            [HParser (Type MaybeQualified)]
        )

typeVar :: HParser (Type MaybeQualified)
typeVar = TypeVar <$> alphaVarName

unit :: HParser (Type MaybeQualified)
unit = UnitType <$ symbol "()"

namedType :: HParser (Type MaybeQualified)
namedType = UserDefinedType <$> typeName

maybeQualified :: HParser (Maybe ModuleName -> b) -> HParser b
maybeQualified p = do
    moduleQualification <- optional (moduleName <* symbol ".")
    t <- p
    pure (t moduleQualification)