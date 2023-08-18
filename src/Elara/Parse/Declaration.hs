module Elara.Parse.Declaration where

import Control.Lens (view, (^.), _1)

import Data.Generics.Wrapped
import Elara.AST.Frontend (FrontendDeclaration, FrontendExpr, FrontendPattern, FrontendTypeDeclaration)
import Elara.AST.Generic
import Elara.AST.Name (ModuleName, Name (..), VarName)
import Elara.AST.Region
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Expression (element)
import Elara.Parse.Indents (exprBlock)
import Elara.Parse.Names (alphaVarName, unqualifiedTypeName, unqualifiedVarName)
import Elara.Parse.Pattern (patParser)
import Elara.Parse.Primitives (HParser, fmapLocated, located, token_)
import Elara.Parse.Type (type', typeNotApplication)
import HeadedMegaparsec (endHead, wrapToHead)
import Text.Megaparsec (choice)

declaration :: Located ModuleName -> HParser FrontendDeclaration
declaration n = choice @[] [defDec n, letDec n, typeDeclaration n]

defDec :: Located ModuleName -> HParser FrontendDeclaration
defDec modName = fmapLocated Declaration $ do
    token_ TokenDef
    endHead
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

letDec :: Located ModuleName -> HParser FrontendDeclaration
letDec modName = fmapLocated Declaration $ do
    (name, patterns, e) <- letRaw
    let valueLocation = sconcat (e ^. _Unwrapped . _1 . sourceRegion :| (view (_Unwrapped . _1 . sourceRegion) <$> patterns))
        value = DeclarationBody (Located valueLocation (Value e patterns NoFieldValue))
    pure (Declaration' modName (NVarName <$> name) value)

letRaw :: HParser (Located VarName, [FrontendPattern], FrontendExpr)
letRaw = wrapToHead $ do
    token_ TokenLet
    endHead
    name <- located unqualifiedVarName
    patterns <- many patParser
    token_ TokenEquals
    e <- exprBlock element
    pure (name, patterns, e)

typeDeclaration :: Located ModuleName -> HParser FrontendDeclaration
typeDeclaration modName = fmapLocated Declaration $ do
    token_ TokenType
    endHead
    isAlias <- isJust <$> optional (token_ TokenAlias)
    name <- located unqualifiedTypeName
    args <- many (located alphaVarName)
    token_ TokenEquals
    body <- located (if isAlias then alias else adt)
    let valueLocation = name ^. sourceRegion <> body ^. sourceRegion
        value = DeclarationBody $ Located valueLocation (TypeDeclaration args body)
    pure (Declaration' modName (NTypeName <$> name) value)

-- | ADT declarations
adt :: HParser FrontendTypeDeclaration
adt =
    ADT <$> (constructor `sepBy1'` token_ TokenPipe)
  where
    constructor = do
        name <- located unqualifiedTypeName
        args <- many typeNotApplication
        pure (name, args)

alias :: HParser FrontendTypeDeclaration
alias = Alias <$> type'
