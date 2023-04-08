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
import Elara.Parse.Names (unqualifiedNormalVarName, unqualifiedTypeName, unqualifiedVarName)
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (HParser, fmapLocated, located, token')
import Elara.Parse.Type (type')
import HeadedMegaparsec (endHead)
import Text.Megaparsec (choice)

declaration :: Located ModuleName -> HParser Frontend.Declaration
declaration n = choice @[] [defDec n, letDec n, typeDeclaration n]

defDec :: Located ModuleName -> HParser Frontend.Declaration
defDec modName = fmapLocated Declaration $ do
  token' TokenDef
  endHead
  name <- located (NVarName <$> unqualifiedVarName)

  token' TokenColon
  typeAnnotation <- located type'

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
  token' TokenLet
  endHead
  name <- located unqualifiedVarName
  patterns <- many pattern'
  token' TokenEquals
  e <- block element
  pure (name, patterns, e)

typeDeclaration :: Located ModuleName -> HParser Frontend.Declaration
typeDeclaration modName = fmapLocated Declaration $ do
  token' TokenType
  endHead
  name <- located unqualifiedTypeName
  args <- many (located unqualifiedNormalVarName)
  token' TokenEquals
  body <- located typeDeclaration'
  let
    valueLocation = name ^. sourceRegion <> body ^. sourceRegion
    value = DeclarationBody $ Located valueLocation (TypeDeclaration args body)
  pure (Declaration' modName (NTypeName <$> name) value)

typeDeclaration' :: HParser Frontend.TypeDeclaration
typeDeclaration' = adt <|> alias

adt :: HParser Frontend.TypeDeclaration
adt =
  Frontend.ADT <$> (constructor `sepBy1'` token' TokenPipe)
 where
  constructor = do
    name <- located unqualifiedTypeName
    args <- many (located type')
    pure (name, args)

alias :: HParser Frontend.TypeDeclaration
alias = Frontend.Alias <$> located type'