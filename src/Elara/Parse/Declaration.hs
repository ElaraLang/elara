module Elara.Parse.Declaration where

import Elara.AST.Frontend (LocatedExpr)
import Elara.Data.Module (Declaration (Declaration), DeclarationBody (Value))
import Elara.Data.Name (ModuleName (ModuleName))
import Elara.Data.TypeAnnotation (TypeAnnotation)
import Elara.Parse.Indents (optionallyIndented)
import Elara.Parse.Name (varName)
import Elara.Parse.Pattern (pattern)
import Elara.Parse.Primitives
import Text.Megaparsec
  ( sepBy,
  )
import Text.Megaparsec.Char

declaration :: ModuleName -> Parser (Declaration LocatedExpr TypeAnnotation (Maybe ModuleName))
declaration = valueDecl

valueDecl :: ModuleName -> Parser (Declaration LocatedExpr TypeAnnotation (Maybe ModuleName))
valueDecl modName = do
  ((name, patterns), e) <- optionallyIndented letPreamble
  let decl = Declaration modName name patterns (Value e Nothing)
  return decl
  where
    letPreamble = do
      _ <- lexeme (string "let")
      name <- varName
      patterns <- sepBy (lexeme pattern) sc
      _ <- lexeme (char '=')
      return (name, patterns)
