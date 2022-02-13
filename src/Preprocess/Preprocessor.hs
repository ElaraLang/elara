module Preprocess.Preprocessor where

import qualified Interpreter.AST as I
import qualified Parse.AST as P
import Preprocess.Expression

preprocess :: P.Line -> I.Line
preprocess (P.ExpressionL e) = I.ExpressionLine $ preprocessExpression e
