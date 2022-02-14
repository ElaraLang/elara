module Preprocess.Expression where

import qualified Interpreter.AST as I
import qualified Parse.AST as P
import Preprocess.Constant

preprocessExpression :: P.Expression -> I.Expression
preprocessExpression (P.ConstE c) = I.Constant (preprocessConst c)
preprocessExpression (P.LetE (P.FunctionP i a) val) = lambdaDesugar i a val
preprocessExpression (P.LetE pattern val) = I.Bind (preprocessPattern pattern) (preprocessExpression val)
preprocessExpression (P.BlockE body) = I.Block (preprocessExpression <$> body)
preprocessExpression (P.FuncApplicationE f val) = I.FunctionApplication (preprocessExpression f) (preprocessExpression val)
preprocessExpression (P.IdentifierE i) = I.Reference (preprocessIdent i)
preprocessExpression (P.ListE elems) = I.List (preprocessExpression <$> elems)
preprocessExpression s = error $ "Cannot preprocess expression: " ++ show s


lambdaDesugar :: P.Identifier -> [P.Pattern] -> P.Expression -> I.Expression
lambdaDesugar fName args body = I.Bind (I.IdentifierPattern $ preprocessIdent fName) (desugarWithoutName args body)


desugarWithoutName :: [P.Pattern] -> P.Expression -> I.Expression
desugarWithoutName [] body = preprocessExpression body
desugarWithoutName [arg] body = I.Lambda (preprocessPattern arg) (preprocessExpression body)
desugarWithoutName (arg : args) body = I.Lambda (preprocessPattern arg) (desugarWithoutName args body)

preprocessPattern :: P.Pattern -> I.Pattern
preprocessPattern (P.IdentifierP i) = I.IdentifierPattern (preprocessIdent i)
-- The parser will parse let f a b = ... as let F(A(B))
preprocessPattern (P.FunctionP i a) = undefined


preprocessIdent :: P.Identifier -> I.Identifier
preprocessIdent (P.NormalIdentifier i) = I.SimpleIdentifier i
preprocessIdent (P.OpIdentifier i) = I.OperatorIdentifier i
