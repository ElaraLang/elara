module Interpreter.Value where

data Value = IntValue Integer
            | StringValue String
            | ListValue [Value] 
