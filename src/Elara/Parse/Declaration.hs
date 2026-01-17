module Elara.Parse.Declaration where

import Data.Generics.Wrapped
import Elara.AST.Frontend (FrontendDeclaration, FrontendTypeDeclaration)
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (ModuleName)
import Elara.AST.Region
import Elara.AST.Select
import Elara.Data.Pretty (Pretty (..))
import Elara.Lexer.Token (Token (..))
import Elara.Logging (logDebug)
import Elara.Parse.Annotation (annotations)
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Expression (letPreamble)
import Elara.Parse.Names
import Elara.Parse.Primitives (Parser, fmapLocated, ignoringIndents, located, token_)
import Elara.Parse.Type (type', typeNotApplication)
import Text.Megaparsec (MonadParsec (notFollowedBy), choice, try)

declaration :: Located ModuleName -> Parser FrontendDeclaration
declaration n = do
    annotations <- annotations
    choice
        [ letDec n annotations
        , defDec n annotations
        , typeDeclaration n annotations
        ]

defDec :: Located ModuleName -> [Annotation Frontend] -> Parser FrontendDeclaration
defDec modName annotations = fmapLocated Declaration $ do
    try (token_ TokenDef)

    name <- located unqualifiedVarName

    token_ TokenColon
    typeAnnotation <- type'

    let annotationLocation = view sourceRegion name <> view (_Unwrapped % _1 % sourceRegion) typeAnnotation
    let annotations' = ValueDeclAnnotations annotations
    let declBody = Located annotationLocation $ ValueTypeDef name typeAnnotation annotations'
    pure
        ( Declaration'
            modName
            (DeclarationBody declBody)
        )

letDec :: Located ModuleName -> [Annotation Frontend] -> Parser FrontendDeclaration
letDec modName annotations = fmapLocated Declaration $ do
    (name, patterns, e) <- letPreamble
    let valueLocation = sconcat (e ^. _Unwrapped % _1 % sourceRegion :| (view (_Unwrapped % _1 % sourceRegion) <$> patterns))
        annotations' = ValueDeclAnnotations annotations
        value = DeclarationBody (Located valueLocation (Value name e patterns NoFieldValue annotations'))
    pure (Declaration' modName value)

typeDeclaration :: Located ModuleName -> [Annotation Frontend] -> Parser FrontendDeclaration
typeDeclaration modName annotations = fmapLocated Declaration $ ignoringIndents $ do
    try (token_ TokenType)

    name <- located conId
    args <- many (located varId)
    token_ TokenEquals
    body <- located (try (alias <* notFollowedBy (token_ TokenPipe)) <|> adt) -- Alias must come first, as otherwise ADT would consume it
    let valueLocation =
            name ^. sourceRegion
                <> body ^. sourceRegion

        annotations' = TypeDeclAnnotations NoFieldValue annotations
        value =
            DeclarationBody $
                Located
                    valueLocation
                    (TypeDeclaration name args body annotations')
    lift $
        logDebug $
            "Body location for type declaration "
                <> pretty (name ^. unlocated)
                <> " at "
                <> pretty (body ^. sourceRegion)
                <> " with name location at "
                <> pretty (name ^. sourceRegion)
                <> " so valueLocation = "
                <> pretty valueLocation
    pure (Declaration' modName value)

-- | ADT declarations
adt :: Parser FrontendTypeDeclaration
adt =
    ADT
        <$> ( optional (token_ TokenPipe) -- allow leading pipe
                *> constructor `sepBy1'` token_ TokenPipe
            )
  where
    constructor = do
        name <- located conId
        args <- many typeNotApplication
        pure (name, args)

alias :: Parser FrontendTypeDeclaration
alias = Alias <$> type'
