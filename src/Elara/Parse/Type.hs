module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Elara.AST.Frontend (Type (..))
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Unqualified (Unqualified), VarName)
import Elara.AST.Region (Located)
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Names (alphaVarName, moduleName, typeName, unqualifiedVarName, varName)
import Elara.Parse.Primitives (HParser, inBraces, located, token')
import Text.Megaparsec (choice)
import Prelude hiding (Type)

type' :: HParser Type
type' =
    makeExprParser
        typeTerm
        [ [InfixR constructorApplication]
        , [InfixR functionType]
        ]

constructorApplication :: HParser (Type -> Type -> Type)
constructorApplication = TypeConstructorApplication <$ pass

functionType :: HParser (Type -> Type -> Type)
functionType = FunctionType <$ token' TokenRightArrow

typeTerm :: HParser Type
typeTerm =
    choice @[]
        [ typeVar
        , unit
        , namedType
        , recordType
        , inBraces type'
        ]

typeVar :: HParser Type
typeVar = TypeVar <$> alphaVarName

unit :: HParser Type
unit = UnitType <$ (token' TokenLeftParen *> token' TokenRightParen)

namedType :: HParser Type
namedType = UserDefinedType <$> located typeName

maybeQualified :: HParser (Maybe ModuleName -> b) -> HParser b
maybeQualified p = do
    moduleQualification <- optional (moduleName <* token' TokenDot)
    t <- p
    pure (t moduleQualification)

recordType :: HParser Type
recordType = inBraces $ do
    fields <- sepBy1' recordField (token' TokenComma)
    pure $ RecordType fields
  where
    recordField :: HParser (Located (Unqualified VarName), Type)
    recordField = do
        name <- located unqualifiedVarName
        token' TokenColon
        t <- type'
        pure (name, t)