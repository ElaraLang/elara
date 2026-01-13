module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Generics.Wrapped
import Data.List.NonEmpty ((<|))
import Elara.AST.Frontend (FrontendType, FrontendType')
import Elara.AST.Generic (Type (..), Type' (..))
import Elara.AST.Generic.Common (NoFieldValue (NoFieldValue))
import Elara.AST.Name (LowerAlphaName)
import Elara.AST.Region (Located (..), enclosingRegion', sourceRegion)
import Elara.Data.AtLeast2List (AtLeast2List (AtLeast2List))
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Error (ElaraParseError (EmptyRecord))
import Elara.Parse.Names
import Elara.Parse.Primitives (Parser, inBraces, inParens, located, locatedTokens', token_)
import Text.Megaparsec (MonadParsec (try), choice, customFailure, (<?>))

type' :: Parser FrontendType
type' =
    makeExprParser
        typeTerm
        [ [InfixL constructorApplication]
        , [InfixR functionType]
        ]
        <?> "type"

typeNotApplication :: Parser FrontendType
typeNotApplication =
    makeExprParser
        typeTerm
        [ [InfixR functionType]
        ]

locatedType :: Parser FrontendType' -> Parser FrontendType
locatedType = (Type . (,NoFieldValue) <$>) . located

constructorApplication :: Parser (FrontendType -> FrontendType -> FrontendType)
constructorApplication = liftedBinaryType pass (const TypeConstructorApplication)

functionType :: Parser (FrontendType -> FrontendType -> FrontendType)
functionType = liftedBinaryType (token_ TokenRightArrow) (const FunctionType)

liftedBinaryType op f = do
    op' <- op
    let create l r =
            let region = enclosingRegion' (l ^. _Unwrapped % _1 % sourceRegion) (r ^. _Unwrapped % _1 % sourceRegion)
             in Located region (f op' l r) ^. to (Type . (,NoFieldValue))
    pure create

typeTerm :: Parser FrontendType
typeTerm =
    choice
        [ typeVar <?> "type variable"
        , namedType <?> "type constructor"
        , try tupleType <?> "tuple type"
        , try (inParens type') <?> "parenthesised type"
        , unit <?> "unit type"
        , emptyRecordError <?> "empty record type"
        , recordType <?> "record type"
        , listType <?> "list type"
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
    pure $ TupleType (AtLeast2List.fromHeadAndTail t ts)

listType :: Parser FrontendType
listType = locatedType $ do
    token_ TokenLeftBracket
    t <- type'
    token_ TokenRightBracket
    pure $ ListType t
