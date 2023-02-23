module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Elara.AST.Frontend (Type (..))
import Elara.AST.Name (MaybeQualified, ModuleName)
import Elara.Parse.Names (alphaVarName, moduleName, typeName)
import Elara.Parse.Primitives (HParser, lexeme, sc, symbol, located)
import Text.Megaparsec (choice)
import Prelude hiding (Type)

type' :: HParser (Type)
type' =
    makeExprParser
        typeTerm
        [ [InfixR constructorApplication]
        , [InfixR functionType]
        ]

constructorApplication :: HParser (Type -> Type -> Type)
constructorApplication = lexeme (TypeConstructorApplication <$ (sc :: HParser ()))

functionType :: HParser (Type -> Type -> Type)
functionType = lexeme (FunctionType <$ (symbol "->" :: HParser ()))

typeTerm :: HParser Type
typeTerm =
    choice
        ( lexeme
            <$> [ typeVar
                , unit
                , namedType
                ] ::
            [HParser Type]
        )

typeVar :: HParser Type
typeVar = TypeVar <$> alphaVarName

unit :: HParser Type
unit = UnitType <$ symbol "()"

namedType :: HParser Type
namedType = UserDefinedType <$> located typeName

maybeQualified :: HParser (Maybe ModuleName -> b) -> HParser b
maybeQualified p = do
    moduleQualification <- optional (moduleName <* symbol ".")
    t <- p
    pure (t moduleQualification)