module Interpreter.Execute where

import Control.Monad
import Data.IORef
import Data.Map ((!), (!?))
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace (traceShowM)
import Interpreter.AST

-- Type of an element in Elara that can be executed. This takes a value (typically a function parameter), an environment, and returns an IO action
type ElaraExecute a = a -> Environment -> IO (Maybe Value)

class Execute a where
  execute :: ElaraExecute a

instance Execute Line where
  execute (ExpressionLine e) = execute e

instance Execute Expression where
  execute (Constant val) _ = case val of
    IntC i -> return $ Just $ IntValue i
    StringC b -> return $ Just $ StringValue b
    _ -> return Nothing
  execute (List elems) s = do
    vals <- mapM (`execute` s) elems
    when (any isNothing vals) $ error $ "List contains expressions that do not produce a value: " ++ show (zip elems vals)
    return $ Just $ ListValue $ map (\(Just v) -> v) vals
  execute (Block expressions) state = do
    results <- mapM (`execute` state) expressions
    return $ last results
  execute (Bind pat val) state = do
    let name = show pat
    val' <- execute val state
    case val' of
      Just val'' -> do
        modifyIORef (bindings state) (M.insert name val'')
        return $ Just val''
      Nothing -> return Nothing
  execute (Lambda arg body) state = do
    let paramName = show arg
    let function = FunctionValue state arg $ createFunction body paramName
    return $ Just function
  execute (Reference i) env = do
    let ident = show i
    bindingMap <- readIORef (bindings env)
    let val = fromMaybe (error ("Could not find binding with name " ++ ident)) (bindingMap !? ident)
    return $ Just val
  execute (FunctionApplication a b) state = do
    aVal <- execute a state
    let (Just (FunctionValue closure _ func)) = aVal
    bVal <- execute b state
    let (Just i) = bVal
    func i closure
  execute a _ = error $ "Not implemented for " ++ show a

createFunction :: Expression -> String -> ElaraExecute Value
createFunction body paramName paramValue state = do
  stateBindings <- readIORef $ bindings state
  let newBindings = M.insert paramName paramValue stateBindings
  newBindingsRef <- newIORef newBindings
  traceShowM newBindings
  let stateWithParamValue = state {bindings = newBindingsRef}
  execute body stateWithParamValue

newtype Environment = Environment {bindings :: IORef (M.Map String Value)}

primitiveFunctions :: IO (M.Map String Value)
primitiveFunctions = do
  emptyEnv <- emptyEnvironment
  return $
    M.fromList
      [ ("println", FunctionValue emptyEnv (IdentifierPattern $ SimpleIdentifier "value") println),
        ("+", FunctionValue emptyEnv (IdentifierPattern $ OperatorIdentifier "+") elaraPlus1)
      ]

elaraPlus1 :: ElaraExecute Value
elaraPlus1 left s = do
  -- let (+) a b =
  let (IntValue a) = left
  return $
    Just $
      FunctionValue s (IdentifierPattern $ SimpleIdentifier "b") $ \right _ -> do
        let (IntValue b) = right
        let result = a + b
        return $ Just $ IntValue result

println :: ElaraExecute Value
println value _ = do
  print value
  return $ Just UnitValue

initialEnvironment :: IO Environment
initialEnvironment = do
  ref <- primitiveFunctions >>= newIORef
  return
    Environment
      { bindings = ref
      }

emptyEnvironment :: IO Environment
emptyEnvironment = do
  ref <- newIORef M.empty
  return
    Environment
      { bindings = ref
      }

data Value
  = IntValue Integer
  | StringValue String
  | ListValue [Value]
  | UnitValue
  | FunctionValue Environment Pattern (ElaraExecute Value)

instance Show Value where
  show (IntValue i) = show i
  show (StringValue s) = s
  show (ListValue l) = show l
  show UnitValue = "()"
  show (FunctionValue _ arg _) = "<function:" ++ show arg ++ ">"
