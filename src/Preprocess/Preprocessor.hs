module Preprocess.Preprocessor where

import Control.Monad.State
import Data.List (partition)
import qualified Interpreter.AST as I
import qualified Parse.AST as P
import Preprocess.Expression
import Preprocess.Type

type ProcessState = State [P.Line] [I.Line]

type Processor = State [P.Line] I.Line

preprocessTypeDef :: P.TypeDef -> I.TypeDefinition
preprocessTypeDef (P.TypeDef name args body) =
  I.TypeDefinition (I.TypeIdentifier . show $ name) (I.TypeVariable . show <$> args) (preprocessTypeDefBody body)
  where
    preprocessTypeDefBody :: P.TypeDefBody -> I.TypeDefinitionBody
    preprocessTypeDefBody (P.AliasType t) = I.AliasType (preprocessType t)
    preprocessTypeDefBody (P.TypeVariableType t) = I.TypeVariableType (I.TypeVariable . show $ t)
    preprocessTypeDefBody (P.UnionType a b) = I.UnionType (preprocessTypeDefBody a) (preprocessTypeDefBody b)
    preprocessTypeDefBody (P.TypeConstructor name fields) = I.TypeConstructor (I.TypeIdentifier . show $ name) (preprocessTypeDefBody <$> fields)

preprocessLine :: P.Line -> Processor
preprocessLine (P.DefL i t) = return $ I.DefLine (preprocessIdent i) (preprocessType t)
preprocessLine (P.TypeDefL typeDef) = return $ I.TypeDefLine $ preprocessTypeDef typeDef
preprocessLine (P.ExpressionL e) = do
  codeLines <- get
  let (expressions, statements) = partition isExpression codeLines
  let (expr, remainingExpressions) = runState preprocessExpression (newState $ e : (toExpr <$> expressions))

  put (statements ++ (P.ExpressionL <$> expressionQueue remainingExpressions))
  return $ I.ExpressionLine expr
  where
    toExpr (P.ExpressionL a) = a
    toExpr _ = error "Expected expression"
    isExpression (P.ExpressionL _) = True
    isExpression _ = False

preprocessLines :: ProcessState
preprocessLines = do
  lines <- get
  case lines of
    [] -> return []
    line : others -> do
      put others
      processed <- preprocessLine line
      rest <- preprocessLines
      return $ processed : rest

preprocessAll :: [P.Line] -> [I.Line]
preprocessAll = evalState preprocessLines
