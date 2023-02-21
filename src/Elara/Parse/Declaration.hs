module Elara.Parse.Declaration where

import Elara.AST.Frontend (TypeAnnotation (TypeAnnotation), _Expr, _Pattern)
import Elara.AST.Module (Declaration (..), Declaration'(..), DeclarationBody(..), DeclarationBody' (Value, ValueTypeDef))
import Elara.AST.Name (ModuleName, Name (..))
import Elara.AST.Select (Frontend)
import Elara.Parse.Expression (letRaw)
import Elara.Parse.Names (varName)
import Elara.Parse.Primitives (Parser, lexeme, sc, symbol, located, fmapLocated)
import Elara.Parse.Type (type')
import Elara.AST.Region
import Control.Lens (view)
import Text.Megaparsec.Char.Lexer qualified as L

declaration :: Located ModuleName -> Parser (Declaration Frontend)
declaration = liftA2 (<|>) defDec letDec

defDec :: Located ModuleName -> Parser (Declaration Frontend)
defDec modName = fmapLocated Declaration $ do
  name <- located $ do
     symbol "def"
     NVarName <$> lexeme varName

  ty <- located $ do
     symbol ":"
     TypeAnnotation name <$> type'
  
  let annotationLocation = spanningRegion (view _SourceRegion name :| [view _SourceRegion ty])
  let declBody = Located annotationLocation $ ValueTypeDef (Just <$> ty)
  pure (Declaration' modName name (DeclarationBody declBody))

letDec :: Located ModuleName -> Parser (Declaration Frontend)
letDec modName = fmapLocated Declaration $ do
  (name, patterns, e) <- L.nonIndented sc letRaw
  let 
    valueLocation = spanningRegion (view (_Expr . _SourceRegion) e :| (view (_Pattern . _SourceRegion) <$> patterns))
    value = DeclarationBody $ Located valueLocation (Value e patterns Nothing)
  pure (Declaration' modName (NVarName <$> name) value)
