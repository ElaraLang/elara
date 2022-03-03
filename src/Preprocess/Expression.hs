module Preprocess.Expression (preprocessExpression, preprocessIdent) where

import Control.Monad.State
import qualified Interpreter.AST as I
import qualified Parse.AST as P
import Preprocess.Constant
import Debug.Trace (traceShowM, traceShowId)

type ExpProcessor = State [P.Expression] I.Expression

pull :: State [P.Expression] P.Expression
pull = do
  expressions <- get
  case expressions of
    [] -> error "pull: empty list"
    (x : xs) -> do
      put xs
      return x

preprocessExpression :: ExpProcessor
preprocessExpression = do
  e <- pull
  preprocessExpression' e
  where
    preprocessExpression' :: P.Expression -> ExpProcessor
    preprocessExpression' (P.ConstE c) = return $ I.Constant (preprocessConst c)
    preprocessExpression' (P.ConsE a b) = do
      a' <- preprocessExpression' a
      b' <- preprocessExpression' b
      return $ I.Cons a' b'
    preprocessExpression' (P.IfElseE a b c) = do
      a' <- preprocessExpression' a
      b' <- preprocessExpression' b
      c' <- preprocessExpression' c
      return $ I.IfElse a' b' c'
    --preprocessExpression (P.LetE (P.FunctionP i a) val) = lambdaDesugar i a val
    preprocessExpression' (P.LetE pattern val) = do
      val' <- preprocessExpression' val
      body <- preprocessExpression
      return $ I.BindWithBody (preprocessPattern pattern) val' body
    preprocessExpression' (P.BlockE body) = I.Block <$> mapM preprocessExpression' body
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

--lambdaDesugar :: P.Identifier -> [P.Pattern] -> P.Expression -> I.Expression
--lambdaDesugar fName args body = I.BindWithBody (I.IdentifierPattern $ preprocessIdent fName) (desugarWithoutName args body)

--desugarWithoutName :: [P.Pattern] -> P.Expression -> I.Expression
--desugarWithoutName [] body = preprocessExpression body
--desugarWithoutName [arg] body = I.Lambda (preprocessPattern arg) (preprocessExpression body)
--desugarWithoutName (arg : args) body = I.Lambda (preprocessPattern arg) (desugarWithoutName args body)

preprocessPattern :: P.Pattern -> I.Pattern
preprocessPattern (P.IdentifierP i) = I.IdentifierPattern (preprocessIdent i)
preprocessPattern (P.ConsP a b) = I.ConsPattern (preprocessPattern a) (preprocessPattern b)
preprocessPattern (P.ConstantP c) = I.ConstantPattern (preprocessConst c)
preprocessPattern P.WildP = I.WildcardPattern
preprocessPattern (P.ListP elems) = I.ListPattern (preprocessPattern <$> elems)
preprocessPattern (P.FunctionP _ _) = error "Function pattern should not exist anymore"

preprocessIdent :: P.Identifier -> I.Identifier
preprocessIdent (P.NormalIdentifier i) = I.SimpleIdentifier i
preprocessIdent (P.OpIdentifier i) = I.OperatorIdentifier i
