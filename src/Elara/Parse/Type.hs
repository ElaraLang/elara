module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.List.NonEmpty ((<|))

import Data.Generics.Wrapped
import Elara.AST.Frontend (FrontendType, FrontendType')
import Elara.AST.Generic (Type (..), Type' (..))
import Elara.AST.Name (ModuleName, VarName)
import Elara.AST.Region (Located (..))
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (liftedBinary, sepBy1')
import Elara.Parse.Error (ElaraParseError (EmptyRecord))
import Elara.Parse.Names (alphaVarName, moduleName, typeName, unqualifiedVarName)
import Elara.Parse.Primitives (HParser, IsParser (fromParsec), inBraces, inParens, inParens', located, locatedTokens', token_)
import HeadedMegaparsec (endHead)
import Text.Megaparsec (choice, customFailure)

type' :: HParser FrontendType
type' =
    makeExprParser
        typeTerm
        [ [InfixL constructorApplication]
        , [InfixR functionType]
        ]

typeNotApplication :: HParser FrontendType
typeNotApplication =
    makeExprParser
        typeTerm
        [ [InfixR functionType]
        ]

locatedType :: HParser FrontendType' -> HParser FrontendType
locatedType = (Type <$>) . located

constructorApplication :: HParser (FrontendType -> FrontendType -> FrontendType)
constructorApplication = liftedBinary pass (const TypeConstructorApplication) _Unwrapped

functionType :: HParser (FrontendType -> FrontendType -> FrontendType)
functionType = liftedBinary (token_ TokenRightArrow) (const FunctionType) _Unwrapped

typeTerm :: HParser FrontendType
typeTerm =
    choice @[]
        [ typeVar
        , unit
        , inParens type'
        , tupleType
        , namedType
        , emptyRecordError
        , recordType
        , listType
        ]

typeVar :: HParser FrontendType
typeVar =
    locatedType $
        TypeVar <$> located alphaVarName

unit :: HParser FrontendType
unit =
    locatedType $
        UnitType <$ (token_ TokenLeftParen *> token_ TokenRightParen)

namedType :: HParser FrontendType
namedType =
    locatedType $ UserDefinedType <$> located typeName

maybeQualified :: HParser (Maybe ModuleName -> b) -> HParser b
maybeQualified p = do
    moduleQualification <- optional (moduleName <* token_ TokenDot)
    t <- p
    pure (t moduleQualification)

recordType :: HParser FrontendType
recordType = locatedType $ inBraces $ do
    fields <- sepBy1' recordField (token_ TokenComma)
    pure $ RecordType fields
  where
    recordField :: HParser (Located VarName, FrontendType)
    recordField = do
        name <- located unqualifiedVarName
        token_ TokenColon
        t <- type'
        pure (name, t)

emptyRecordError :: HParser FrontendType
emptyRecordError = do
    sr <- locatedTokens' (TokenLeftBrace :| [TokenRightBrace])
    endHead
    fromParsec $ customFailure (EmptyRecord sr)

tupleType :: HParser FrontendType
tupleType = locatedType $ inParens' $ do
    t <- type'
    token_ TokenComma
    endHead
    ts <- sepBy1' type' (token_ TokenComma)
    pure $ TupleType (t <| ts)

listType :: HParser FrontendType
listType = locatedType $ do
    token_ TokenLeftBracket
    t <- type'
    token_ TokenRightBracket
    pure $ ListType t
