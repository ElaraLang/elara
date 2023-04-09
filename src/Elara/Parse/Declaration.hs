module Elara.Parse.Declaration where

import Control.Lens (view, (^.))
import Elara.AST.Frontend (Declaration (..), Declaration' (..), DeclarationBody (..), DeclarationBody' (..), Expr, Pattern, _Expr, _Pattern)
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Name (ModuleName, Name (..), VarName)
import Elara.AST.Region
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Expression (element)
import Elara.Parse.Indents (block)
import Elara.Parse.Names (alphaVarName, unqualifiedTypeName, unqualifiedVarName)
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (HParser, fmapLocated, located, token_)
import Elara.Parse.Type (type', typeNotApplication)
import HeadedMegaparsec (endHead)
import Text.Megaparsec (choice)

declaration :: Located ModuleName -> HParser Frontend.Declaration
declaration n = choice @[] [defDec n, letDec n, typeDeclaration n]

defDec :: Located ModuleName -> HParser Frontend.Declaration
defDec modName = fmapLocated Declaration $ do
  token_ TokenDef
  endHead
  name <- located (NVarName <$> unqualifiedVarName)

  token_ TokenColon
  typeAnnotation <- type'

  let annotationLocation = view sourceRegion name <> view sourceRegion typeAnnotation
  let declBody = Located annotationLocation $ Frontend.ValueTypeDef typeAnnotation
  pure (Declaration' modName name (DeclarationBody declBody))

letDec :: Located ModuleName -> HParser Frontend.Declaration
letDec modName = fmapLocated Declaration $ do
  (name, patterns, e) <- letRaw
  let
    valueLocation = mconcat (e ^. _Expr . sourceRegion : (view (_Pattern . sourceRegion) <$> patterns))
    value = DeclarationBody $ Located valueLocation (Frontend.Value e patterns)
  pure (Declaration' modName (NVarName <$> name) value)

letRaw :: HParser (Located VarName, [Pattern], Expr)
letRaw = do
  token_ TokenLet
  endHead
  name <- located unqualifiedVarName
  patterns <- many pattern'
  token_ TokenEquals
  e <- block element
  pure (name, patterns, e)

typeDeclaration :: Located ModuleName -> HParser Frontend.Declaration
typeDeclaration modName = fmapLocated Declaration $ do
  token_ TokenType
  endHead
  isAlias <- isJust <$> optional (token_ TokenAlias)
  name <- located unqualifiedTypeName
  args <- many (located alphaVarName)
  token_ TokenEquals
  body <- located (if isAlias then alias else adt)
  let
    valueLocation = name ^. sourceRegion <> body ^. sourceRegion
    value = DeclarationBody $ Located valueLocation (TypeDeclaration args body)
  pure (Declaration' modName (NTypeName <$> name) value)

-- | ADT declarations
adt :: HParser Frontend.TypeDeclaration
adt =
  Frontend.ADT <$> (constructor `sepBy1'` token_ TokenPipe)
 where
  constructor = do
    name <- located unqualifiedTypeName
    args <- many typeNotApplication
    pure (name, args)

alias :: HParser Frontend.TypeDeclaration
alias = Frontend.Alias <$> type'