module Elara.Parse.Declaration where

import Control.Lens (from, view, (^.))
import Elara.AST.Frontend (Declaration (..), Declaration' (..), DeclarationBody (..), Expr, Pattern, TypeAnnotation (TypeAnnotation), _Expr, _Pattern)
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Name (MaybeQualified, ModuleName, Name (..), Unqualified, VarName, _Unqualified)
import Elara.AST.Region
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Expression (exprParser)
import Elara.Parse.Indents (block)
import Elara.Parse.Names (unqualifiedVarName, varName)
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (HParser, fmapLocated, located, token')
import Elara.Parse.Type (type')
import HeadedMegaparsec (endHead)

declaration :: Located ModuleName -> HParser Frontend.Declaration
declaration = liftA2 (<|>) defDec letDec

defDec :: Located ModuleName -> HParser Frontend.Declaration
defDec modName = fmapLocated Declaration $ do
  name <- located $ do
    token' TokenDef
    endHead
    NVarName <$> unqualifiedVarName

  ty <- located $ do
    token' TokenColon
    TypeAnnotation name <$> type'

  let annotationLocation = spanningRegion' (view sourceRegion name :| [view sourceRegion ty])
  let declBody = Located annotationLocation $ Frontend.ValueTypeDef ty
  pure (Declaration' modName name (DeclarationBody declBody))

letDec :: Located ModuleName -> HParser Frontend.Declaration
letDec modName = fmapLocated Declaration $ do
  (name, patterns, e) <- letRaw
  let
    valueLocation = spanningRegion' (view (_Expr . sourceRegion) e :| (view (_Pattern . sourceRegion) <$> patterns))
    value = DeclarationBody $ Located valueLocation (Frontend.Value e patterns)
  pure (Declaration' modName (NVarName <$> name) value)

letRaw :: HParser (Located VarName, [Pattern], Expr)
letRaw = do
  token' TokenLet
  endHead
  name <- located unqualifiedVarName
  patterns <- many pattern'
  token' TokenEquals
  e <- block exprParser
  pure (name, patterns, e)