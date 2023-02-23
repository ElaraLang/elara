module Elara.Parse.Declaration where

import Control.Lens (view)
import Elara.AST.Frontend (TypeAnnotation (TypeAnnotation), _Expr, _Pattern)
import Elara.AST.Module (Declaration (..), Declaration' (..), DeclarationBody (..), DeclarationBody' (Value, ValueTypeDef))
import Elara.AST.Name (ModuleName, Name (..))
import Elara.AST.Region
import Elara.AST.Select (Frontend)
import Elara.Parse.Expression (letRaw)
import Elara.Parse.Indents (nonIndented)
import Elara.Parse.Names (varName)
import Elara.Parse.Primitives (HParser, fmapLocated, lexeme, located, sc, symbol)
import Elara.Parse.Type (type')
import HeadedMegaparsec (endHead)

declaration :: Located ModuleName -> HParser (Declaration Frontend)
declaration = liftA2 (<|>) defDec letDec

defDec :: Located ModuleName -> HParser (Declaration Frontend)
defDec modName = fmapLocated Declaration $ do
  name <- located $ do
    symbol "def"
    endHead
    NVarName <<$>> lexeme varName

  ty <- located $ do
    symbol ":"
    TypeAnnotation name <$> type'

  let annotationLocation = spanningRegion (view _SourceRegion name :| [view _SourceRegion ty])
  let declBody = Located annotationLocation $ ValueTypeDef (Just <$> ty)
  pure (Declaration' modName name (DeclarationBody declBody))

letDec :: Located ModuleName -> HParser (Declaration Frontend)
letDec modName = fmapLocated Declaration $ do
  (name, patterns, e) <- snd <$> nonIndented sc letRaw
  let
    valueLocation = spanningRegion (view (_Expr . _SourceRegion) e :| (view (_Pattern . _SourceRegion) <$> patterns))
    value = DeclarationBody $ Located valueLocation (Value e patterns Nothing)
  pure (Declaration' modName (NVarName <<$>> name) value)