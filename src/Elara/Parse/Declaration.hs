module Elara.Parse.Declaration where

import Control.Applicative.Combinators (sepBy)
import Elara.AST.Frontend (Expr, Pattern, TypeAnnotation (TypeAnnotation))
import Elara.AST.Module (Declaration (..), DeclarationBody (Value, ValueTypeDef))
import Elara.AST.Name (MaybeQualified, ModuleName, Name (..), VarName)
import Elara.AST.Select (Frontend)
import Elara.Parse.Expression (element, exprParser, letRaw)
import Elara.Parse.Indents (optionallyIndented)
import Elara.Parse.Names (varName)
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (Parser, lexeme, sc, symbol)
import Elara.Parse.Type (type')
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

declaration :: ModuleName -> Parser (Declaration Frontend)
declaration = liftA2 (<|>) defDec letDec

defDec :: ModuleName -> Parser (Declaration Frontend)
defDec modName = do
  symbol "def"
  name <- NVarName <$> lexeme varName
  symbol ":"
  ty <- type'
  let annotation = TypeAnnotation name ty
  let declBody = ValueTypeDef (Just annotation)
  pure (Declaration modName name declBody)

letDec :: ModuleName -> Parser (Declaration Frontend)
letDec modName = do
  (name, patterns, e) <- L.nonIndented sc letRaw
  let value = Value e patterns Nothing
  pure (Declaration modName (NVarName name) value)
