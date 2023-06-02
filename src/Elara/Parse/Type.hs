module Elara.Parse.Type where

import Control.Lens (view)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.List.NonEmpty ((<|))
import Elara.AST.Frontend (Type (..))
import Elara.AST.Name (ModuleName, VarName)
import Elara.AST.Region (Located (..), sourceRegion)
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Error (ElaraParseError (EmptyRecord))
import Elara.Parse.Names (alphaVarName, moduleName, typeName, unqualifiedVarName)
import Elara.Parse.Primitives (HParser, IsParser (fromParsec), inBraces, inParens, inParens', located, locatedTokens', token_)
import HeadedMegaparsec (endHead)
import Text.Megaparsec (choice, customFailure)

type' :: HParser (Located Type)
type' =
    makeExprParser
        typeTerm
        [ [InfixL constructorApplication]
        , [InfixR functionType]
        ]

typeNotApplication :: HParser (Located Type)
typeNotApplication =
    makeExprParser
        typeTerm
        [ [InfixR functionType]
        ]

constructorApplication :: HParser (Located Type -> Located Type -> Located Type)
constructorApplication =
    pure
        ( \t1 t2 ->
            Located
                (view sourceRegion t1 <> view sourceRegion t2)
                (TypeConstructorApplication t1 t2)
        )

functionType :: HParser (Located Type -> Located Type -> Located Type)
functionType = do
    token_ TokenRightArrow
    pure
        ( \t1 t2 ->
            Located
                (view sourceRegion t1 <> view sourceRegion t2)
                (FunctionType t1 t2)
        )

typeTerm :: HParser (Located Type)
typeTerm =
    choice @[]
        [ located typeVar
        , located unit
        , inParens type'
        , located tupleType
        , located namedType
        , located emptyRecordError
        , located recordType
        ]

typeVar :: HParser Type
typeVar = TypeVar <$> located alphaVarName

unit :: HParser Type
unit = UnitType <$ (token_ TokenLeftParen *> token_ TokenRightParen)

namedType :: HParser Type
namedType = UserDefinedType <$> located typeName

maybeQualified :: HParser (Maybe ModuleName -> b) -> HParser b
maybeQualified p = do
    moduleQualification <- optional (moduleName <* token_ TokenDot)
    t <- p
    pure (t moduleQualification)

recordType :: HParser Type
recordType = inBraces $ do
    fields <- sepBy1' recordField (token_ TokenComma)
    pure $ RecordType fields
  where
    recordField :: HParser (Located VarName, Located Type)
    recordField = do
        name <- located unqualifiedVarName
        token_ TokenColon
        t <- type'
        pure (name, t)

emptyRecordError :: HParser Type
emptyRecordError = do
    sr <- locatedTokens' (TokenLeftBrace :| [TokenRightBrace])
    endHead
    fromParsec $ customFailure (EmptyRecord sr)

tupleType :: HParser Type
tupleType = inParens' $ do
    t <- type'
    token_ TokenComma
    endHead
    ts <- sepBy1' type' (token_ TokenComma)
    pure $ TupleType (t <| ts)
