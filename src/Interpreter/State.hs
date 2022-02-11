module Interpreter.State where

import Data.IORef (IORef, newIORef)
import qualified Data.Map as M
import Interpreter.Value

data Environment = Environment
  { bindings :: IORef (M.Map String Value)
  }

primitiveFunctions :: M.Map String Value
primitiveFunctions = M.fromList [("println", FunctionValue printLn)]

printLn :: Value -> IO Value
printLn value = do
  print value
  return UnitValue

initialEnvironment :: IO Environment
initialEnvironment = do
  ref <- newIORef primitiveFunctions
  return
    Environment
      { bindings = ref
      }
