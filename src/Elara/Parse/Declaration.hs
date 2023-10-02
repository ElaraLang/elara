module Elara.Parse.Declaration where

import Control.Lens (view, (^.), _1)
import Data.Generics.Wrapped
import Elara.AST.Frontend (FrontendDeclaration, FrontendTypeDeclaration)
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (ModuleName, Name (..))
import Elara.AST.Region
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Expression (letPreamble)
import Elara.Parse.Names
import Elara.Parse.Primitives (Parser, fmapLocated, located, token_)
import Elara.Parse.Type (type', typeNotApplication)
import Text.Megaparsec (choice)

declaration :: Located ModuleName -> Parser FrontendDeclaration
declaration n =
    choice
        [ letDec n
        , defDec n
        , typeDeclaration n
        ]

defDec :: Located ModuleName -> Parser FrontendDeclaration
defDec modName = fmapLocated Declaration $ do
    token_ TokenDef

    name <- located (NVarName <$> unqualifiedVarName)

    token_ TokenColon
    typeAnnotation <- type'

    let annotationLocation = view sourceRegion name <> view (_Unwrapped . sourceRegion) typeAnnotation
    let declBody = Located annotationLocation $ ValueTypeDef typeAnnotation
    pure
        ( Declaration'
            modName
            name
            ( DeclarationBody declBody
            )
        )

letDec :: Located ModuleName -> Parser FrontendDeclaration
letDec modName = fmapLocated Declaration $ do
    (name, patterns, e) <- letPreamble
    let valueLocation = sconcat (e ^. _Unwrapped . _1 . sourceRegion :| (view (_Unwrapped . _1 . sourceRegion) <$> patterns))
        value = DeclarationBody (Located valueLocation (Value e patterns NoFieldValue))
    pure (Declaration' modName (NVarName <$> name) value)

typeDeclaration :: Located ModuleName -> Parser FrontendDeclaration
typeDeclaration modName = fmapLocated Declaration $ do
    token_ TokenType

    isAlias <- isJust <$> optional (token_ TokenAlias)
    name <- located conId
    args <- many (located varId)
    token_ TokenEquals
    body <- located (if isAlias then alias else adt)
    let valueLocation = name ^. sourceRegion <> body ^. sourceRegion
        value = DeclarationBody $ Located valueLocation (TypeDeclaration args body)
    pure (Declaration' modName (NTypeName <$> name) value)

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
