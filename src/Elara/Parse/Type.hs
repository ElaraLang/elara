module Elara.Parse.Type where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Elara.AST.Extensions (TupleTypeExtension (..))
import Elara.AST.Name (LowerAlphaName)
import Elara.AST.Phases.Frontend
import Elara.AST.Region (Located (..), SourceRegion, enclosingRegion')
import Elara.AST.Types
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Error (ElaraParseError (EmptyRecord))
import Elara.Parse.Names
import Elara.Parse.Primitives (Parser, inBraces, inParens, located, locatedTokens', token_)
import Text.Megaparsec (MonadParsec (try), choice, customFailure, (<?>))

typeRegion :: FrontendType -> SourceRegion
typeRegion (Type loc _ _) = loc

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
locatedType p = (\(Located sr node) -> Type sr () node) <$> located p

constructorApplication :: Parser (FrontendType -> FrontendType -> FrontendType)
constructorApplication = liftedBinaryType pass (const TApp)

functionType :: Parser (FrontendType -> FrontendType -> FrontendType)
functionType = liftedBinaryType (token_ TokenRightArrow) (const TFun)

liftedBinaryType :: Parser op -> (op -> FrontendType -> FrontendType -> FrontendType') -> Parser (FrontendType -> FrontendType -> FrontendType)
liftedBinaryType op f = do
    op' <- op
    let create l r =
            let region = enclosingRegion' (typeRegion l) (typeRegion r)
             in Type region () (f op' l r)
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
        TVar <$> located varId

unit :: Parser FrontendType
unit =
    locatedType $
        TUnit <$ (token_ TokenLeftParen *> token_ TokenRightParen)

namedType :: Parser FrontendType
namedType =
    locatedType $ TUserDefined <$> located conName

recordType :: Parser FrontendType
recordType = locatedType $ inBraces $ do
    fields <- sepBy1' recordField (token_ TokenComma)
    pure $ TRecord fields
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
    pure $ TExtension (TupleType (AtLeast2List.fromHeadAndTail t ts))

listType :: Parser FrontendType
listType = locatedType $ do
    token_ TokenLeftBracket
    t <- type'
    token_ TokenRightBracket
    pure $ TList t
