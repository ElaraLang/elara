module Elara.Parse.Declaration where

import Elara.AST.Frontend (TypeAnnotation (TypeAnnotation))
import Elara.AST.Module (Declaration (..), DeclarationBody (Value, ValueTypeDef))
import Elara.AST.Name (ModuleName, Name (..))
import Elara.AST.Select (Frontend)
import Elara.Parse.Expression (letRaw)
import Elara.Parse.Indents (nonIndented)
import Elara.Parse.Names (varName)
import Elara.Parse.Primitives (HParser, lexeme, sc, symbol)
import Elara.Parse.Type (type')
import HeadedMegaparsec (endHead)

declaration :: ModuleName -> HParser (Declaration Frontend)
declaration = liftA2 (<|>) defDec letDec

defDec :: ModuleName -> HParser (Declaration Frontend)
defDec modName = do
  symbol "def"
  endHead
  name <- NVarName <$> lexeme varName
  symbol ":"
  ty <- type'
  let annotation = TypeAnnotation name ty
  let declBody = ValueTypeDef (Just annotation)
  pure (Declaration modName name declBody)

letDec :: ModuleName -> HParser (Declaration Frontend)
letDec modName = do
  (name, patterns, e) <- snd <$> nonIndented sc letRaw
  let value = Value e patterns Nothing
  pure (Declaration modName (NVarName name) value)
