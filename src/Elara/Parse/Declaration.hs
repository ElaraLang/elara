module Elara.Parse.Declaration where

import Elara.AST.Frontend (LocatedExpr, Pattern)
import Elara.AST.Generic (PatternLike (patternNames))
import Elara.Data.Module (Declaration (..), DeclarationBody (..), mapExpr)
import Elara.Data.Name (ModuleName)
import Elara.Data.TypeAnnotation (TypeAnnotation (TypeAnnotation))
import Elara.Parse.Expression (element)
import Elara.Parse.Indents (optionallyIndented)
import Elara.Parse.Name (varName)
import Elara.Parse.Name qualified as Name
import Elara.Parse.Pattern (pattern')
import Elara.Parse.Primitives (Parser, lexeme, sc, symbol)
import Elara.Parse.Type (type')
import Text.Megaparsec
  ( sepBy,
  )
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

type FrontendDecl = Declaration LocatedExpr Pattern TypeAnnotation (Maybe ModuleName)

declaration :: ModuleName -> Parser FrontendDecl
declaration = liftA2 (<|>) defDecl valueDecl

defDecl :: ModuleName -> Parser FrontendDecl
defDecl modName = do
  symbol "def"
  name <- lexeme varName
  symbol ":"
  ty <- type'
  let annotation = TypeAnnotation name ty
  let declBody = ValueTypeDef annotation
  pure (Declaration modName name declBody)

valueDecl :: ModuleName -> Parser FrontendDecl
valueDecl modName = L.nonIndented sc $ do
  ((name, patterns), e) <- optionallyIndented letPreamble element
  let names = patterns >>= patternNames
  let promote = fmap (Name.promoteAll names)
      value = Value e patterns Nothing
  let y x =
        3 in pure y
  pure (Declaration modName name (mapExpr promote value))
  where
    letPreamble = do
      symbol "let"
      name <- lexeme varName
      patterns <- sepBy (lexeme pattern') sc
      symbol "="
      pure (name, patterns)