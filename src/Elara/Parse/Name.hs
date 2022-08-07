module Elara.Parse.Name (varName, typeName, opName, moduleName, alphaVarName, promoteArguments) where

import Data.Text qualified as T
import Elara.AST.Frontend (Expr)
import Elara.AST.Frontend qualified as Ast
import Elara.Data.Name (ModuleName, Name, NameFromText)
import Elara.Data.Name qualified as Name
import Elara.Parse.Primitives (Parser, inParens, lexeme)
import Text.Megaparsec
    ( (<|>), optional, oneOf, many, sepEndBy1, some )
import Text.Megaparsec.Char
    ( alphaNumChar, char, lowerChar, upperChar )
import Prelude hiding (some, many)

varName :: Parser Name
varName = qualified varName'
  where
    varName' = lexeme (inParens opName) <|> lexeme alphaVarName

alphaVarName :: (NameFromText n) => Parser n
alphaVarName = Name.fromString <$> lexeme ((:) <$> lowerChar <*> many alphaNumChar)

typeName :: Parser Name
typeName = qualified (Name.fromString <$> capitalizedString)

capitalizedString :: Parser String
capitalizedString = lexeme $ do
  x <- upperChar
  xs <- many alphaNumChar
  return (x : xs)

opName :: Parser Name
opName = qualified opName'
  where
    opName' :: Parser Name
    opName' = Name.fromString <$> lexeme (some operatorChar)
    operatorChar = oneOf ("!#$%&*+./<=>?@\\^|-~" :: String)

moduleName :: Parser ModuleName
moduleName = do
  parts <- sepEndBy1 capitalizedString (char '.') :: Parser [String]
  return (Name.ModuleName (T.pack <$> parts))

qualified :: Parser Name -> Parser Name
qualified parser = do
  module' <- optional moduleName
  Name.withModule module' <$> parser

-- | Turns a Var into an Argument, if necessary
-- | This is used to reference things like lambda parameters by name without needing to lookup the names in the global scope
promoteArguments :: [Name.Name] -> Expr a -> Expr a
promoteArguments allArgs arg = case arg of
  Ast.Var v ->
    case Name.moduleName v of
      Nothing -> if v `elem` allArgs then Ast.Argument v else arg
      _ -> arg
  _ -> arg