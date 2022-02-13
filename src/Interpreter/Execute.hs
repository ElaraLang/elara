module Interpreter.Execute where

import Data.IORef
import Data.Map ((!))
import qualified Data.Map as M
import Parse.AST


-- Type of an element in Elara that can be executed. This takes a value (typically a function parameter), an environment, and returns an IO action
type ElaraExecute a = a -> Environment -> IO (Maybe Value)

class Execute a where
  execute :: ElaraExecute a

instance Execute Line where
  execute (ExpressionL e) = execute e

instance Execute Expression where
  execute (ConstE val) _ = case val of
    IntC i -> return $ Just $ IntValue i
    StringC b -> return $ Just $ StringValue b
    _ -> return Nothing
  execute (BlockE expressions) state = do
    results <- mapM (`execute` state) expressions
    return $ last results
  execute (LetE (IdentifierP p) val) state = do
    let name = identifierValue p
    val' <- execute val state
    case val' of
      Just val'' -> do
        modifyIORef (bindings state) (M.insert name val'')
        return $ Just val''
      Nothing -> return Nothing
  execute (LetE (FunctionP i args) body) state = do
    let name = identifierValue i
    let paramName = showPattern $ head args 
    let function = FunctionValue $ createFunction body paramName
    modifyIORef (bindings state) (M.insert name function)
    return $ Just function
  execute (IdentifierE i) env = do
    let ident = identifierValue i
    bindingMap <- readIORef (bindings env)
    let val = bindingMap ! ident
    return $ Just val
  execute (FuncApplicationE a b) state = do
    aVal <- execute a state
    let (Just (FunctionValue func)) = aVal
    bVal <- execute b state
    let (Just i) = bVal
    func i state
  execute a _ = error $ "Not implemented for " ++ show a

createFunction ::Expression -> String -> ElaraExecute Value
createFunction body paramName paramValue state = do
  stateBindings <- readIORef $ bindings state
  let newBindings = M.insert paramName paramValue stateBindings
  newBindingsRef <- newIORef newBindings
  let stateWithParamValue = state {bindings = newBindingsRef}
  execute body stateWithParamValue

newtype Environment = Environment {bindings :: IORef (M.Map String Value)}

primitiveFunctions :: M.Map String Value
primitiveFunctions = M.fromList [("println", FunctionValue printLn)]

printLn :: ElaraExecute Value
printLn value _ = do
  print value
  return $ Just UnitValue

initialEnvironment :: IO Environment
initialEnvironment = do
  ref <- newIORef primitiveFunctions
  return
    Environment
      { bindings = ref
      }

data Value
  = IntValue Integer
  | StringValue String
  | ListValue [Value]
  | UnitValue
  | FunctionValue (ElaraExecute Value)

instance Show Value where
  show (IntValue i) = show i
  show (StringValue s) = s
  show (ListValue l) = show l
  show UnitValue = "()"
  show (FunctionValue _) = "<function>"
