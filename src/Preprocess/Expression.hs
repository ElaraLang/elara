module Preprocess.Expression (preprocessExpression, preprocessIdent, newState, expressionQueue) where

import Control.Monad.State
import Data.List.NonEmpty (toList)
import Debug.Trace (traceShowM)
import qualified Interpreter.AST as I
import qualified Parse.AST as P
import Preprocess.Constant

type ExpProcessor = State ExpState I.Expression

data ExpState = ExpState
  { expressionQueue :: [P.Expression]
  }
  deriving (Show)

newState :: [P.Expression] -> ExpState
newState expressionQueue' =
  ExpState
    { expressionQueue = expressionQueue'
    }

pull :: State ExpState P.Expression
pull = do
  s <- get
  case expressionQueue s of
    [] -> error "pull: empty list"
    (x : xs) -> do
      put $ s {expressionQueue = xs}
      return x

preprocessExpression :: ExpProcessor
preprocessExpression = do
  e <- pull
  preprocessExpression' e

preprocessExpression' :: P.Expression -> ExpProcessor
preprocessExpression' (P.ConstE c) = return $ I.Constant (preprocessConst c)
preprocessExpression' (P.FixE e) = I.Fix <$> preprocessExpression' e
preprocessExpression' (P.LambdaE x e) = do
  e' <- preprocessExpression' e
  return $ I.Lambda (I.SimpleIdentifier $ show x) e'
preprocessExpression' (P.ConsE a b) = do
  a' <- preprocessExpression' a
  b' <- preprocessExpression' b
  return $ I.Cons a' b'
preprocessExpression' (P.IfElseE a b c) = do
  a' <- preprocessExpression' a
  b' <- preprocessExpression' b
  c' <- preprocessExpression' c
  return $ I.IfElse a' b' c'
preprocessExpression' (P.LetE (P.FunctionP i a) val) = lambdaDesugar i a val -- Desugars let a b = ... into let a = \b -> ...
preprocessExpression' (P.LetE ident val) = do
  let (I.IdentifierPattern pattern) = preprocessPattern ident
  val' <- preprocessExpression' val
  return $ I.BindGlobal pattern val'
preprocessExpression' (P.BlockE body) = desugarBlock (toList body)
preprocessExpression' (P.FuncApplicationE f val) = do
  f' <- preprocessExpression' f
  val' <- preprocessExpression' val
  return $ I.FunctionApplication f' val'
preprocessExpression' (P.IdentifierE i) = return $ I.Reference (preprocessIdent i)
preprocessExpression' (P.ListE elems) = I.List <$> mapM preprocessExpression' elems
preprocessExpression' (P.InfixApplicationE op l r) = do
  l' <- preprocessExpression' l
  r' <- preprocessExpression' r
  return $ I.FunctionApplication (I.FunctionApplication (I.Reference $ preprocessIdent op) l') r'
preprocessExpression' (P.MatchE e cases) = do
  e' <- preprocessExpression' e
  cases' <- mapM preprocessCase cases
  return $ I.Match e' cases'
  where
    preprocessCase (P.MatchLine i a) = do
      a' <- preprocessExpression' a
      return $ I.MatchCase (preprocessPattern i) a'
preprocessExpression' s = error $ "Cannot preprocess expression: " ++ show s

lambdaDesugar :: P.Identifier -> [P.Pattern] -> P.Expression -> State ExpState I.Expression
lambdaDesugar fName args body = do
  let lambdaBody = foldr P.LambdaE body (P.IdentifierP fName : args)
  let newLet = P.LetE (P.IdentifierP fName) (P.FixE lambdaBody)
  preprocessExpression' newLet

preprocessPattern :: P.Pattern -> I.Pattern
preprocessPattern (P.IdentifierP i) = I.IdentifierPattern (preprocessIdent i)
preprocessPattern (P.ConsP a b) = I.ConsPattern (preprocessPattern a) (preprocessPattern b)
preprocessPattern (P.ConstantP c) = I.ConstantPattern (preprocessConst c)
preprocessPattern P.WildP = I.WildcardPattern
preprocessPattern (P.ListP elems) = I.ListPattern (preprocessPattern <$> elems)
preprocessPattern (P.FunctionP _ _) = error "Function pattern should not exist anymore"

desugarBlock :: [P.Expression] -> State ExpState I.Expression
desugarBlock exps = desugarBlock' exps []
  where
    desugarBlock' (P.LetE ident val : others) acc = do
      let (I.IdentifierPattern pat) = preprocessPattern ident
      val' <- preprocessExpression' val
      body <- desugarBlock others
      return $! I.Block (acc ++ [I.BindWithBody pat (I.Fix val') body])
    desugarBlock' [] [acc] = return acc
    desugarBlock' [] acc = return $ I.Block $! reverse acc
    desugarBlock' (e : exs) acc = do
      t <- preprocessExpression' e
      desugarBlock' exs (t : acc)

preprocessIdent :: P.Identifier -> I.Identifier
preprocessIdent (P.NormalIdentifier i) = I.SimpleIdentifier i
preprocessIdent (P.OpIdentifier i) = I.OperatorIdentifier i
