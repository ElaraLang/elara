module Elara.Parse.Declaration where

import Control.Lens (over, set, transform, transformOn, view)
import Control.Monad (liftM2)
import Elara.AST.Frontend (Expr, LocatedExpr, Pattern)
import Elara.AST.Generic (PatternLike (patternNames))
import Elara.Data.Located
import Elara.Data.Module (Declaration (..), DeclarationBody (..), HasBody (body), mapExpr)
import Elara.Data.Name (ModuleName)
import Elara.Data.TypeAnnotation (TypeAnnotation (TypeAnnotation))
import Elara.Parse.Expression (expression)
import Elara.Parse.Indents (optionallyIndented)
import Elara.Parse.Name (varName)
import Elara.Parse.Name qualified as Name
import Elara.Parse.Pattern (pattern)
import Elara.Parse.Primitives (Parser, lexeme, sc, symbol)
import Elara.Parse.Type (type')
import Text.Megaparsec
  ( sepBy,
    try,
    (<|>),
  )

type FrontendDecl = Declaration LocatedExpr Pattern TypeAnnotation (Maybe ModuleName)

declaration :: ModuleName -> Parser FrontendDecl
declaration = liftM2 (<|>) valueDecl defDecl

defDecl :: ModuleName -> Parser FrontendDecl
defDecl modName = do
  symbol "def"
  name <- varName
  symbol ":"
  ty <- type'
  let annotation = TypeAnnotation name ty
  let declBody = ValueTypeDef annotation
  return (Declaration modName name declBody)

valueDecl :: ModuleName -> Parser FrontendDecl
valueDecl modName = do
  ((name, patterns), e) <- optionallyIndented letPreamble expression
  let names = patterns >>= patternNames
  let promote = fmap (transform (Name.promoteArguments names))
      value = Value e patterns Nothing
      dec = Declaration modName name (mapExpr promote value)

  return dec
  where
    letPreamble = do
      symbol "let"
      name <- varName
      patterns <- sepBy (lexeme pattern) sc
      symbol "="
      return (name, patterns)