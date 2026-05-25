module Elara.Parse.Declaration where

import Text.Megaparsec (MonadParsec (notFollowedBy), choice, try)

import Elara.AST.Location
import Elara.AST.Name (ModuleName, VarName)
import Elara.AST.Phase (ElaraPhase (..), NoExtension (..))
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
import Elara.Parse.Primitives (Parser, ignoringIndents, located, taggedLocated, token_)
import Elara.Parse.Type (type', typeNotApplication)

exprRegion :: Expr loc p -> NodeLoc ExprNode loc
exprRegion (Expr loc _ _) = loc

patternRegion :: Pattern SourceRegion p -> NodeLoc PatternNode SourceRegion
patternRegion (Pattern loc _ _) = loc

typeRegion :: Type SourceRegion p -> NodeLoc TypeNode SourceRegion
typeRegion (Type loc _ _) = loc

locatedDecl :: Parser (Declaration' SourceRegion Frontend) -> Parser FrontendDeclaration
locatedDecl p = (\(Located sr inner) -> Declaration (DeclLoc sr) inner) <$> located p

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

    name <- tagLocated <$> located unqualifiedVarName

    token_ TokenColon
    typeAnnotation <- type'

    let nameLoc = getLocation name
    let annotationLocation = tag DeclNode (nameLoc <.> typeRegion typeAnnotation)
    let declBody = DeclarationBody annotationLocation (DeclBodyExtension (FrontendValueTypeDef name typeAnnotation anns))
    pure
        ( Declaration'
            (tagLocated modName)
            declBody
        )

letDec :: Located ModuleName -> [Annotation SourceRegion Frontend] -> Parser FrontendDeclaration
letDec modName anns = locatedDecl $ do
    (name, patterns, e) <- letPreambleParser
    let nameLoc = getLocation name
    let exprLoc = exprRegion e
    let patternsLoc = spanAs @PatternNode (map patternRegion patterns)
    let valueLocation = nameLoc <.> exprLoc <.> patternsLoc
        value = DeclarationBody valueLocation (ValueDeclaration name e patterns Nothing NoExtension anns)
    pure (Declaration' (tagLocated modName) value)

typeDeclaration :: Located ModuleName -> [Annotation SourceRegion Frontend] -> Parser FrontendDeclaration
typeDeclaration modName anns = locatedDecl $ ignoringIndents $ do
    try (token_ TokenType)

    name <- taggedLocated TypeNode conId
    typeVars <- many (located varId)
    token_ TokenEquals
    body <- located (try (alias <* notFollowedBy (token_ TokenPipe)) <|> adt)

    let typeVarsLoc = spanAs @TypeNode typeVars
    let nameRegion = getLocation name
    let bodyRegion = tagLocated @TypeNode body
    let valueLocation = nameRegion <.> typeVarsLoc <.> getLocation bodyRegion

        value =
            DeclarationBody
                (widen valueLocation)
                (TypeDeclarationBody name (fmap tagLocated typeVars) (body ^. unlocated) Nothing NoExtension anns)
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
    pure (Declaration' (tagLocated modName) value)

-- | ADT declarations
adt :: Parser FrontendTypeDeclaration
adt =
    ADT
        <$> ( optional (token_ TokenPipe)
                *> constructor `sepBy1'` token_ TokenPipe
            )
  where
    constructor = do
        name <- tagLocated @TypeNode <$> located conId
        args <- many typeNotApplication
        pure (name, args)

alias :: Parser FrontendTypeDeclaration
alias = Alias <$> type'
