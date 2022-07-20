module Elara.Parse.Declaration where

import Control.Monad (liftM2)
import Elara.AST.Frontend (LocatedExpr)
import Elara.Data.Module (Declaration (Declaration), DeclarationBody (Value, ValueTypeDef))
import Elara.Data.Name (ModuleName)
import Elara.Data.TypeAnnotation (TypeAnnotation (TypeAnnotation))
import Elara.Parse.Indents (optionallyIndented)
import Elara.Parse.Name (varName)
import Elara.Parse.Pattern (pattern)
import Elara.Parse.Primitives
import Elara.Parse.Type (type')
import Text.Megaparsec
  ( sepBy,
    try,
    (<|>),
  )
import Text.Megaparsec.Char

declaration :: ModuleName -> Parser (Declaration LocatedExpr TypeAnnotation (Maybe ModuleName))
declaration = liftM2 ((<|>) . try) defDecl valueDecl

defDecl :: ModuleName -> Parser (Declaration LocatedExpr TypeAnnotation (Maybe ModuleName))
defDecl modName = do
  _ <- lexeme (string "def")
  name <- varName
  _ <- lexeme (char ':')
  ty <- type'
  let annotation = TypeAnnotation name ty
  let declBody = ValueTypeDef annotation
  return (Declaration modName name declBody)

valueDecl :: ModuleName -> Parser (Declaration LocatedExpr TypeAnnotation (Maybe ModuleName))
valueDecl modName = do
  ((name, patterns), e) <- optionallyIndented letPreamble
  return (Declaration modName name (Value e patterns Nothing))
  where
    letPreamble = do
      _ <- lexeme (string "let")
      name <- varName
      patterns <- sepBy (lexeme pattern) sc
      _ <- lexeme (char '=')
      return (name, patterns)