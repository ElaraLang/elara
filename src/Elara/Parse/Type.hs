module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Generics.Wrapped
import Data.List.NonEmpty ((<|))
import Elara.AST.Frontend (FrontendType, FrontendType')
import Elara.AST.Generic (Type (..), Type' (..))
import Elara.AST.Name (LowerAlphaName)
import Elara.AST.Region (Located (..))
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (liftedBinary, sepBy1')
import Elara.Parse.Error (ElaraParseError (EmptyRecord))
import Elara.Parse.Names
import Elara.Parse.Primitives (Parser, inBraces, inParens, located, locatedTokens', token_)
import Text.Megaparsec (choice, customFailure, MonadParsec (try))

type' :: Parser FrontendType
type' =
    makeExprParser
        typeTerm
        [ [InfixL constructorApplication]
        , [InfixR functionType]
        ]

typeNotApplication :: Parser FrontendType
typeNotApplication =
    makeExprParser
        typeTerm
        [ [InfixR functionType]
        ]

locatedType :: Parser FrontendType' -> Parser FrontendType
locatedType = (Type <$>) . located

constructorApplication :: Parser (FrontendType -> FrontendType -> FrontendType)
constructorApplication = liftedBinary pass (const TypeConstructorApplication) _Unwrapped

functionType :: Parser (FrontendType -> FrontendType -> FrontendType)
functionType = liftedBinary (token_ TokenRightArrow) (const FunctionType) _Unwrapped

typeTerm :: Parser FrontendType
typeTerm =
    choice @[]
        [ typeVar
        , try (inParens type')
        , unit
        , tupleType
        , namedType
        , emptyRecordError
        , recordType
        , listType
        ]

typeVar :: Parser FrontendType
typeVar =
    locatedType $
        TypeVar <$> located varId

unit :: Parser FrontendType
unit =
    locatedType $
        UnitType <$ (token_ TokenLeftParen *> token_ TokenRightParen)

namedType :: Parser FrontendType
namedType =
    locatedType $ UserDefinedType <$> located conName

recordType :: Parser FrontendType
recordType = locatedType $ inBraces $ do
    fields <- sepBy1' recordField (token_ TokenComma)
    pure $ RecordType fields
  where
    recordField :: Parser (Located LowerAlphaName, FrontendType)
    recordField = do
        name <- located varId
        token_ TokenColon
        t <- type'
        pure (name, t)

emptyRecordError :: Parser FrontendType
emptyRecordError = do
    sr <- locatedTokens' (TokenLeftBrace :| [TokenRightBrace])
    customFailure (EmptyRecord sr)

tupleType :: Parser FrontendType
tupleType = locatedType $ inParens $ do
    t <- type'
    token_ TokenComma
    ts <- sepBy1' type' (token_ TokenComma)
    pure $ TupleType (t <| ts)

listType :: Parser FrontendType
listType = locatedType $ do
    token_ TokenLeftBracket
    t <- type'
    token_ TokenRightBracket
    pure $ ListType t
