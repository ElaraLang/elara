module Elara.Parse.Declaration where

import Elara.AST.Name (ModuleName)
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Frontend
import Elara.AST.Region (HasSourceRegion (sourceRegion), Located (..), SourceRegion, unlocated)
import Elara.AST.Types
import Elara.Data.Pretty (Pretty (..))
import Elara.Lexer.Token (Token (..))
import Elara.Logging (logDebug)
import Elara.Parse.Annotation (annotations)
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Grammar (exprParser, letPreambleParser)
import Elara.Parse.Names
import Elara.Parse.Primitives (Parser, ignoringIndents, located, token_)
import Elara.Parse.Type (type', typeNotApplication)
import Text.Megaparsec (MonadParsec (notFollowedBy), choice, try)

exprRegion :: Expr SourceRegion p -> SourceRegion
exprRegion (Expr loc _ _) = loc

patternRegion :: Pattern SourceRegion p -> SourceRegion
patternRegion (Pattern loc _ _) = loc

typeRegion :: Type SourceRegion p -> SourceRegion
typeRegion (Type loc _ _) = loc

locatedDecl :: Parser (Declaration' SourceRegion Frontend) -> Parser FrontendDeclaration
locatedDecl p = (\(Located sr inner) -> Declaration sr inner) <$> located p

declaration :: Located ModuleName -> Parser FrontendDeclaration
declaration n = do
    anns <- annotations
    choice
        [ letDec n anns
        , defDec n anns
        , typeDeclaration n anns
        ]

defDec :: Located ModuleName -> [Annotation SourceRegion Frontend] -> Parser FrontendDeclaration
defDec modName anns = locatedDecl $ do
    try (token_ TokenDef)

    name <- located unqualifiedVarName

    token_ TokenColon
    typeAnnotation <- type'

    let Located nameRegion _ = name
    let annotationLocation = nameRegion <> typeRegion typeAnnotation
    let declBody = DeclarationBody annotationLocation (DeclBodyExtension (FrontendValueTypeDef name typeAnnotation anns))
    pure
        ( Declaration'
            modName
            declBody
        )

letDec :: Located ModuleName -> [Annotation SourceRegion Frontend] -> Parser FrontendDeclaration
letDec modName anns = locatedDecl $ do
    (name, patterns, e) <- letPreambleParser
    let valueLocation = name ^. sourceRegion <> exprRegion e <> mconcat (map patternRegion patterns)
        value = DeclarationBody valueLocation (ValueDeclaration name e patterns Nothing NoExtension anns)
    pure (Declaration' modName value)

typeDeclaration :: Located ModuleName -> [Annotation SourceRegion Frontend] -> Parser FrontendDeclaration
typeDeclaration modName anns = locatedDecl $ ignoringIndents $ do
    try (token_ TokenType)

    name <- located conId
    args <- many (located varId)
    token_ TokenEquals
    body <- located (try (alias <* notFollowedBy (token_ TokenPipe)) <|> adt)
    let Located nameRegion _ = name
    let Located bodyRegion _ = body
    let valueLocation = nameRegion <> bodyRegion

        value =
            DeclarationBody
                valueLocation
                (TypeDeclarationBody name args (body ^. unlocated) Nothing NoExtension anns)
    lift $
        logDebug $
            "Body location for type declaration "
                <> pretty (name ^. unlocated)
                <> " at "
                <> pretty bodyRegion
                <> " with name location at "
                <> pretty nameRegion
                <> " so valueLocation = "
                <> pretty valueLocation
    pure (Declaration' modName value)

-- | ADT declarations
adt :: Parser FrontendTypeDeclaration
adt =
    ADT
        <$> ( optional (token_ TokenPipe)
                *> constructor `sepBy1'` token_ TokenPipe
            )
  where
    constructor = do
        name <- located conId
        args <- many typeNotApplication
        pure (name, args)

alias :: Parser FrontendTypeDeclaration
alias = Alias <$> type'
