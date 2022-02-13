module Preprocess.Constant where
  
import qualified Interpreter.AST as I
import qualified Parse.AST as P

preprocessConst :: P.Constant -> I.Constant
preprocessConst (P.IntC i) = I.IntC i
preprocessConst (P.StringC s) = I.StringC s
preprocessConst P.UnitC = I.UnitC