module Elara.Parse.Declaration where

import Control.Monad (liftM2)
import Elara.AST.Frontend (LocatedExpr, Pattern)
import Elara.Data.Module (Declaration (Declaration), DeclarationBody (Value, ValueTypeDef))
import Elara.Data.Name (ModuleName)
import Elara.Data.TypeAnnotation (TypeAnnotation (TypeAnnotation))
import Elara.Parse.Expression (expression)
import Elara.Parse.Indents (indentedBlock, nonIndented, optionallyIndented)
import Elara.Parse.Name (varName)
import Elara.Parse.Pattern (pattern)
import Elara.Parse.Primitives
import Elara.Parse.Type (type')
import Text.Megaparsec
  ( sepBy,
    try,
    (<|>),
  )

type FrontendDecl = Declaration LocatedExpr Pattern TypeAnnotation (Maybe ModuleName)

declaration :: ModuleName -> Parser FrontendDecl
declaration = liftM2 ((<|>) . try) defDecl valueDecl

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
  return (Declaration modName name (Value e patterns Nothing))
  where
    letPreamble = do
      symbol "let"
      name <- varName
      patterns <- sepBy (lexeme pattern) sc
      symbol "="
      return (name, patterns)