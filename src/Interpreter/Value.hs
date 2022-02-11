module Interpreter.Value where

data Value
  = IntValue Integer
  | StringValue String
  | ListValue [Value]
  | UnitValue
  | FunctionValue (Value -> IO Value)

instance Show Value where
  show (IntValue i) = show i
  show (StringValue s) = s
  show (ListValue l) = show l
  show UnitValue = "()"
  show (FunctionValue _) = "<function>"
