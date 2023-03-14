module Elara.Parse.Declaration where

import Control.Lens (view)
import Elara.AST.Frontend (Expr, Pattern, TypeAnnotation (TypeAnnotation), _Expr, _Pattern)
import Elara.AST.Module (Declaration (..), Declaration' (..), DeclarationBody (..), DeclarationBody' (Value, ValueTypeDef))
import Elara.AST.Name (MaybeQualified, ModuleName, Name (..), VarName)
import Elara.AST.Region
import Elara.AST.Select (Frontend)
import Elara.Lexer.Token (Token (..))
import Elara.Parse.Expression (exprParser)
import Elara.Parse.Indents (block)
import Elara.Parse.Names (varName)
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (HParser, fmapLocated, located, token')
import Elara.Parse.Type (type')
import HeadedMegaparsec (endHead)

declaration :: Located ModuleName -> HParser (Declaration Frontend)
declaration = liftA2 (<|>) defDec letDec

defDec :: Located ModuleName -> HParser (Declaration Frontend)
defDec modName = fmapLocated Declaration $ do
  name <- located $ do
    token' TokenDef
    endHead
    NVarName <<$>> varName

  ty <- located $ do
    token' TokenColon
    TypeAnnotation name <$> type'

  let annotationLocation = spanningRegion' (view sourceRegion name :| [view sourceRegion ty])
  let declBody = Located annotationLocation $ ValueTypeDef (Just <$> ty)
  pure (Declaration' modName name (DeclarationBody declBody))

letDec :: Located ModuleName -> HParser (Declaration Frontend)
letDec modName = fmapLocated Declaration $ do
  (name, patterns, e) <- letRaw
  let
    valueLocation = spanningRegion' (view (_Expr . sourceRegion) e :| (view (_Pattern . sourceRegion) <$> patterns))
    value = DeclarationBody $ Located valueLocation (Value e patterns Nothing)
  pure (Declaration' modName (NVarName <<$>> name) value)

letRaw :: HParser (Located (MaybeQualified VarName), [Pattern], Expr)
letRaw = do
  token' TokenLet
  endHead
  name <- located varName
  patterns <- many pattern'
  token' TokenEquals
  e <- block exprParser
  pure (name, patterns, e)