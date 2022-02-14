module Interpreter.Execute where

import Control.Monad
import Data.IORef
import Data.Map ((!), (!?))
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace (traceShowId, trace)
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
  execute (Cons a b) s = do
    (Just a') <- execute a s

    b' <- execute b s
    case b' of
      Just (ListValue l) -> return $ Just $ ListValue $ a' : l
      Just a -> error $ "Cannot cons to a non-list (" ++ show a ++ ")"
      Nothing -> return Nothing
  execute (List elems) s = do
    vals <- filterResults elems <$> mapM (`execute` s) elems
    return $ Just $ ListValue vals
  execute (Block expressions) state = do
    results <- mapM (`execute` state) expressions
    return $ last results
  execute (Bind pat val) state = do
    val' <- execute val state
    case val' of
      Just val'' -> do
        let patternValues = applyPattern val'' pat
        modifyIORef (bindings state) (patternValues `M.union`)
        return $ Just val''
      Nothing -> return Nothing
  execute (Lambda arg body) state = do
    let function = FunctionValue state arg $ createFunction body arg
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
    env <- closure `union` state
    func i env
  execute (IfElse cond ifTrue ifFalse) state = do
    condVal <- execute cond state
    case condVal of
      Just (BoolValue True) -> execute ifTrue state
      Just (BoolValue False) -> execute ifFalse state
      _ -> error "Condition in if statement must be a boolean"
  execute (Match expr cases) state = do
    Just exprVal <- execute expr state

    let matchOption = filter (\(MatchCase pat val) -> matchPattern exprVal pat) cases
    case matchOption of
      (MatchCase pat val) : _ -> do
        stateBindings <- readIORef (bindings state)
        let newBindings = applyPattern exprVal pat `M.union` stateBindings
        ref <- newIORef newBindings
        execute val Environment {bindings = ref}
      [] -> error "Match case not exhaustive"
  execute a _ = error $ "Not implemented for " ++ show a

filterResults :: (Show a) => [a] -> [Maybe Value] -> [Value]
filterResults elems results = map filterNothing results
  where
    filterNothing (Just v) = v
    filterNothing Nothing = error $ "List contains expressions that do not produce a value: " ++ show (zip elems results)

createFunction :: Expression -> Pattern -> ElaraExecute Value
createFunction body paramName paramValue state = do
  stateBindings <- readIORef $ bindings state
  let patterns = applyPattern paramValue paramName
  let newBindings = patterns `M.union` stateBindings
  newBindingsRef <- newIORef newBindings
  let stateWithParamValue = state {bindings = newBindingsRef}
  execute body stateWithParamValue

matchPattern :: Value -> Pattern -> Bool
matchPattern _ (IdentifierPattern _) = True
matchPattern v (ConstantPattern c) = constantToValue c == v
matchPattern (ListValue (a : b)) (ConsPattern a' b') = matchPattern a a' && matchPattern (ListValue b) b'
matchPattern (ListValue l) (ListPattern pats) = length l == length pats && and (zipWith matchPattern l pats)
matchPattern a _ = trace ("Not matching " ++ show a) False

-- Destructures a pattern into its components.
-- This function assumes that matchPattern has already been called to ensure that the pattern matches the value.
applyPattern :: Value -> Pattern -> M.Map String Value
applyPattern v (IdentifierPattern i) = M.singleton (show i) v
applyPattern _ (ConstantPattern _) = M.empty
applyPattern v (ConsPattern a b) = case v of
  ListValue (a' : b') -> do
    let tail' = ListValue b'
    let headPattern = applyPattern a' a
    let tailPattern = applyPattern tail' b
    headPattern `M.union` tailPattern
  ListValue [] -> error "List is empty"
  o -> error $ "Cons pattern applied to non-list " ++ show o
applyPattern (ListValue l) (ListPattern pats) = M.unions $ zipWith applyPattern l pats
applyPattern v p = error $ "Can't apply pattern " ++ show p ++ " to value " ++ show v

newtype Environment = Environment {bindings :: IORef (M.Map String Value)}

primitiveFunctions :: IO (M.Map String Value)
primitiveFunctions = do
  emptyEnv <- emptyEnvironment
  return $
    M.fromList
      [ ("println", FunctionValue emptyEnv (IdentifierPattern $ SimpleIdentifier "value") println),
        ("+", FunctionValue emptyEnv (IdentifierPattern $ OperatorIdentifier "+") elaraPlus1),
        ("==", FunctionValue emptyEnv (IdentifierPattern $ OperatorIdentifier "==") elaraEq)
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

elaraEq :: ElaraExecute Value
elaraEq left s = do
  return $
    Just $
      FunctionValue s (IdentifierPattern $ SimpleIdentifier "b") $ \right _ -> do
        let result = left == right
        return $ Just $ BoolValue result

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

union :: Environment -> Environment -> IO Environment
envA `union` envB = do
  a <- readIORef $ bindings envA
  b <- readIORef $ bindings envB
  newRef <- newIORef $ a `M.union` b
  return Environment {bindings = newRef}

data Value
  = IntValue Integer
  | StringValue String
  | ListValue [Value]
  | UnitValue
  | BoolValue Bool
  | FunctionValue Environment Pattern (ElaraExecute Value)

instance Show Value where
  show (IntValue i) = show i
  show (StringValue s) = s
  show (ListValue l) = show l
  show UnitValue = "()"
  show (BoolValue b) = show b
  show (FunctionValue _ arg _) = "<function:" ++ show arg ++ ">"

instance Eq Value where
  (IntValue a) == (IntValue b) = a == b
  (StringValue a) == (StringValue b) = a == b
  (ListValue a) == (ListValue b) = a == b
  UnitValue == UnitValue = True
  (BoolValue a) == (BoolValue b) = a == b
  f@FunctionValue {} == a = error $ "Cannot compare functions: " ++ show f ++ " == " ++ show a
  a == f@FunctionValue {} = error $ "Cannot compare functions: " ++ show a ++ " == " ++ show f
  _ == _ = False

constantToValue :: Constant -> Value
constantToValue (IntC i) = IntValue i
constantToValue (StringC s) = StringValue s
constantToValue UnitC = UnitValue
