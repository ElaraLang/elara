module Elara.Parse.Declaration where

import Data.Generics.Wrapped
import Elara.AST.Frontend (FrontendDeclaration, FrontendTypeDeclaration)
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (ModuleName, ToName (..))
import Elara.AST.Region
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Error
import Elara.Parse.Expression (letPreamble, operator)
import Elara.Parse.Literal (integerLiteral)
import Elara.Parse.Names
import Elara.Parse.Primitives (Parser, fmapLocated, ignoringIndents, located, token, token_, withPredicate)
import Elara.Parse.Type (type', typeNotApplication)
import Text.Megaparsec (choice)

declaration :: Located ModuleName -> Parser FrontendDeclaration
declaration n =
    choice
        [ letDec n
        , defDec n
        , typeDeclaration n
        , infixDecl n
        ]

defDec :: Located ModuleName -> Parser FrontendDeclaration
defDec modName = fmapLocated Declaration $ do
    token_ TokenDef

    name <- located unqualifiedVarName

    token_ TokenColon
    typeAnnotation <- type'

    let annotationLocation = view sourceRegion name <> view (_Unwrapped % _1 % sourceRegion) typeAnnotation
    let declBody = Located annotationLocation $ ValueTypeDef name typeAnnotation
    pure
        ( Declaration'
            modName
            (DeclarationBody declBody)
        )

letDec :: Located ModuleName -> Parser FrontendDeclaration
letDec modName = fmapLocated Declaration $ do
    (name, patterns, e) <- letPreamble
    let valueLocation = sconcat (e ^. _Unwrapped % _1 % sourceRegion :| (view (_Unwrapped % _1 % sourceRegion) <$> patterns))
        annotations = ValueDeclAnnotations Nothing
        value = DeclarationBody (Located valueLocation (Value name e patterns NoFieldValue annotations))
    pure (Declaration' modName value)

typeDeclaration :: Located ModuleName -> Parser FrontendDeclaration
typeDeclaration modName = fmapLocated Declaration $ ignoringIndents $ do
    token_ TokenType

    isAlias <- isJust <$> optional (token_ TokenAlias)
    name <- located conId
    args <- many (located varId)
    token_ TokenEquals
    body <- located (if isAlias then alias else adt)
    let valueLocation = name ^. sourceRegion <> body ^. sourceRegion
        annotations = TypeDeclAnnotations Nothing NoFieldValue
        value = DeclarationBody $ Located valueLocation (TypeDeclaration name args body annotations)
    pure (Declaration' modName value)

infixDecl :: Located ModuleName -> Parser FrontendDeclaration
infixDecl modName = fmapLocated Declaration $ do
    assoc <- located (LeftAssoc <$ token TokenInfixL <|> RightAssoc <$ token TokenInfixR <|> NonAssoc <$ token TokenInfix)
    prec <-
        withPredicate
            (\prec -> (prec ^. unlocated) >= 0 && (prec ^. unlocated) <= 10)
            InfixPrecTooHigh
            (located integerLiteral)
    name <- located operator
    let valueLocation = sconcat (assoc ^. sourceRegion :| [prec ^. sourceRegion, name ^. sourceRegion])
        value = DeclarationBody $ Located valueLocation (InfixDecl $ InfixDeclaration declName (fromInteger <$> prec) assoc)
        declName = case name ^. unlocated of
            MkBinaryOperator (Located _ (SymOp sym)) -> toName <$> sym
            MkBinaryOperator (Located _ (Infixed x)) -> toName <$> x
    pure (Declaration' modName value)

-- | ADT declarations
adt :: Parser FrontendTypeDeclaration
adt =
    ADT <$> (constructor `sepBy1'` token_ TokenPipe)
  where
    constructor = do
        name <- located conId
        args <- many typeNotApplication
        pure (name, args)

alias :: Parser FrontendTypeDeclaration
alias = Alias <$> type'
