{-# LANGUAGE FlexibleContexts #-}

module Elara.Parse.Name (varName, typeName, opName, moduleName, alphaVarName, promoteArguments, promoteAll) where

import Control.Lens.Plated (transform)
import Elara.AST.Frontend (Expr)
import Elara.AST.Frontend qualified as Ast
import Elara.Data.Located qualified
import Elara.Data.Name (ModuleName (..), Name (..), NameFromText, QualifiedName (..))
import Elara.Data.Name qualified as Name
import Elara.Parse.Combinators (sepBy1')
import Elara.Parse.Primitives (Parser, inParens, lexeme, symbol)
import Text.Megaparsec (
  many,
  oneOf,
  some,
 )
import Text.Megaparsec.Char (
  alphaNumChar,
  lowerChar,
  upperChar,
 )
import Prelude hiding (many, some)

varName :: Parser Name
varName = qualified varName'
 where
  varName' = inParens opName <|> alphaVarName

alphaVarName :: (NameFromText n) => Parser n
alphaVarName = Name.fromString <$> ((:) <$> lowerChar <*> many alphaNumChar)

typeName :: Parser Name
typeName = do
  ModuleName names <- moduleName
  pure $ case names of
    x :| [] -> Name x
    _ -> Qualified $ QualifiedName (ModuleName (fromList $ init names)) (Name (last names))

capitalizedString :: Parser String
capitalizedString = lexeme $ do
  x <- upperChar
  xs <- many alphaNumChar
  pure (x : xs)

opName :: Parser Name
opName = qualified opName'
 where
  opName' :: Parser Name
  opName' = Name.fromString <$> lexeme (some operatorChar)
  operatorChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)

moduleName :: Parser ModuleName
moduleName = do
  parts <- sepBy1' capitalizedString (symbol ".")
  pure (Name.ModuleName (toText <$> parts))

qualified :: Parser Name -> Parser Name
qualified parser = do
  module' <- optional (moduleName <* symbol ".")

  Name.withModule module' <$> parser

{- | Turns a Var into an Argument, if necessary
 | This is used to reference things like lambda parameters by name without needing to lookup the names in the global scope
-}
promoteArguments :: [Name.Name] -> Expr a -> Expr a
promoteArguments allArgs arg = case arg of
  Ast.Var v ->
    case Name.moduleName v of
      Nothing -> if v `elem` allArgs then Ast.Argument v else arg
      _ -> arg
  _ -> arg

promoteAll :: Traversable (Elara.Data.Located.XRec a) => [Name] -> Expr a -> Expr a
promoteAll names = transform (promoteArguments names)