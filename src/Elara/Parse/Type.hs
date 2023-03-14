module Elara.Parse.Type where

import Control.Lens
import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import Elara.AST.Frontend (Type (..))
import Elara.AST.Name (ModuleName, Unqualified, VarName)
import Elara.AST.Region (Located, sourceRegion)
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Error (ElaraParseError (EmptyRecord))
import Elara.Parse.Names (alphaVarName, moduleName, typeName, unqualifiedVarName)
import Elara.Parse.Primitives (HParser, IsParser (fromParsec), inBraces, located, locatedTokens', token')
import HeadedMegaparsec (endHead)
import Text.Megaparsec (choice, customFailure)
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
        , emptyRecordError
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

emptyRecordError :: HParser Type
emptyRecordError = do
    sr <- locatedTokens' (TokenLeftBrace :| [TokenRightBrace])
    endHead
    fromParsec $ customFailure (EmptyRecord sr)
