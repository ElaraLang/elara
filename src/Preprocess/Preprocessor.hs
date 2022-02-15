module Preprocess.Preprocessor where

import qualified Interpreter.AST as I
import qualified Parse.AST as P
import Preprocess.Expression
import Preprocess.Type

preprocess :: P.Line -> I.Line
preprocess (P.ExpressionL e) = I.ExpressionLine $ preprocessExpression e
preprocess (P.DefL pat t) = I.DefLine (preprocessPattern pat) (preprocessType t)
