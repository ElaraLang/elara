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
preprocessExpression' (P.LambdaE x e rec) = do
  e' <- preprocessExpression' e
  return $ I.Lambda (I.SimpleIdentifier $ show x) e' rec
preprocessExpression' (P.ConsE a b) = do
  a' <- preprocessExpression' a
  b' <- preprocessExpression' b
  return $ I.Cons a' b'
preprocessExpression' (P.IfElseE a b c) = do
  a' <- preprocessExpression' a
  b' <- preprocessExpression' b
  c' <- preprocessExpression' c
  return $ I.IfElse a' b' c'
preprocessExpression' (P.LetE ident val) = do
  (pattern, val') <- preprocessLet ident val
  return $ I.BindGlobal (pattern) val'
preprocessExpression' (P.LetInE ident val body) = do
  (pattern, val') <- preprocessLet ident val
  body' <- preprocessExpression' body
  return $ I.BindWithBody (pattern) val' body'
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

preprocessLet :: P.Pattern -> P.Expression -> State ExpState (I.Identifier, I.Expression)
preprocessLet pat val = do
  let fName = P.NormalIdentifier $ patName pat
  let args = case pat of
        (P.FunctionP _ a) -> (P.IdentifierP fName) : a
        _ -> []
  let lambdaBody = foldr (flip flip False . P.LambdaE) val args
  let fixLambdaBody =
        if null args
          then lambdaBody
          else let (P.LambdaE a b False) = lambdaBody in P.FixE (P.LambdaE a b True) -- Make the first argument recursive
  val' <- preprocessExpression' fixLambdaBody
  return (preprocessIdent fName, val')

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
      (pat, val') <- preprocessLet ident val
      body <- desugarBlock others
      return $! I.Block (acc ++ [I.BindWithBody (pat) val' body])
    desugarBlock' [] [acc] = return acc
    desugarBlock' [] acc = return $ I.Block $! reverse acc
    desugarBlock' (e : exs) acc = do
      t <- preprocessExpression' e
      desugarBlock' exs (t : acc)

preprocessIdent :: P.Identifier -> I.Identifier
preprocessIdent (P.NormalIdentifier i) = I.SimpleIdentifier i
preprocessIdent (P.OpIdentifier i) = I.OperatorIdentifier i

patName :: P.Pattern -> String
patName (P.IdentifierP i) = show i
patName (P.FunctionP i _) = show i
patName other = error "Cannot get name of pattern " ++ show other
