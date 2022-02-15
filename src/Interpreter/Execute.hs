module Interpreter.Execute where

import Control.Monad (unless)
import Data.IORef
import Data.Map ((!), (!?))
import qualified Data.Map as M
import Data.Maybe
import Interpreter.AST
import System.CPUTime (getCPUTime)

-- Type of an element in Elara that can be executed. This takes a value (typically a function parameter), an environment, and returns an IO action
type ElaraExecute a = a -> Environment -> IO (Maybe TypedValue)

class Execute a where
  execute :: ElaraExecute a

instance Execute Line where
  execute (ExpressionLine e) s = execute e s
  execute (DefLine pat t) s = do
    env <- readIORef (bindings s)
    -- Create the dummy value
    let val = typeOnlyValue t
    -- Apply the pattern to the dummy value
    let matched = applyPattern val pat
    -- Merge the new bindings into the environment
    let newEnv = M.union matched env
    -- Write the new environment back to the bindings
    writeIORef (bindings s) newEnv
    return Nothing

instance Execute Expression where
  execute (Constant val) _ = case val of
    IntC i -> return $ Just $ inferTypes $ IntValue i
    StringC b -> return $ Just $inferTypes $ StringValue b
    UnitC -> return $ Just $ inferTypes UnitValue
  execute (Cons a b) s = do
    (Just a') <- execute a s

    b' <- execute b s
    case value <$> b' of
      Just (ListValue l) -> return $ Just $ inferTypes $ ListValue $ a' : l
      Just a -> error $ "Cannot cons to a non-list (" ++ show a ++ ")"
      Nothing -> return Nothing
  execute (List elems) s = do
    vals <- filterResults elems <$> mapM (`execute` s) elems
    return $ Just $ inferTypes $ ListValue vals
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
    return $ Just $ inferTypes function
  execute (Reference i) env = do
    let ident = show i
    bindingMap <- readIORef (bindings env)
    let val = fromMaybe (error ("Could not find binding with name " ++ ident)) (bindingMap !? ident)
    return $ Just val
  execute (FunctionApplication a b) state = do
    Just aVal <- execute a state
    let expectedType = case type_ aVal of
          PureFunctionType argType _ -> argType
          _ -> error $ "Cannot apply non-function " ++ show a ++ " (" ++ show aVal ++ ")"
    let (FunctionValue closure _ func) = value aVal
    (Just arg) <- execute b state
    env <- closure `union` state
    unless (expectedType `allows` type_ arg) $ error $ "Expected " ++ show expectedType ++ " but got " ++ show (type_ arg) ++ " for value " ++ show b
    func arg env
  execute (IfElse cond ifTrue ifFalse) state = do
    condVal <- execute cond state
    case value <$> condVal of
      Just (BoolValue True) -> execute ifTrue state
      Just (BoolValue False) -> execute ifFalse state
      _ -> error "Condition in if statement must be a boolean"
  execute (Match expr cases) state = do
    Just matchee <- execute expr state

    let matchOption = filter (\(MatchCase pat _) -> matchPattern matchee pat) cases
    case matchOption of
      (MatchCase pat expression) : _ -> do
        stateBindings <- readIORef (bindings state)
        let patBindings = applyPattern matchee pat
        let newBindings = patBindings `M.union` stateBindings
        ref <- newIORef newBindings

        execute expression Environment {bindings = ref}
      [] -> error "Match case not exhaustive"
  execute a _ = error $ "Not implemented for " ++ show a

filterResults :: (Show a) => [a] -> [Maybe TypedValue] -> [TypedValue]
filterResults elems results = map filterNothing results
  where
    filterNothing (Just v) = v
    filterNothing Nothing = error $ "List contains expressions that do not produce a value: " ++ show (zip elems results)

createFunction :: Expression -> Pattern -> ElaraExecute TypedValue
createFunction body paramName paramValue state = do
  stateBindings <- readIORef $ bindings state
  let patterns = applyPattern paramValue paramName
  let newBindings = patterns `M.union` stateBindings
  newBindingsRef <- newIORef newBindings
  let stateWithParamValue = state {bindings = newBindingsRef}
  execute body stateWithParamValue

matchPattern :: TypedValue -> Pattern -> Bool
matchPattern _ WildcardPattern = True
matchPattern _ (IdentifierPattern _) = True
matchPattern (TypedValue (ListValue (a : b)) (ListType _)) (ConsPattern a' b') = matchPattern a a' && matchPattern (inferTypes $ ListValue b) b'
matchPattern tv p = matchPattern' (value tv) p
  where
    matchPattern' v (ConstantPattern c) = constantToValue c == v
    matchPattern' (ListValue l) (ListPattern pats) = length l == length pats && and (zipWith matchPattern l pats)
    matchPattern' _ _ = False

-- Destructures a pattern into its components.
-- This function assumes that matchPattern has already been called to ensure that the pattern matches the value.
applyPattern :: TypedValue -> Pattern -> M.Map String TypedValue
applyPattern _ WildcardPattern = M.empty
applyPattern _ (ConstantPattern _) = M.empty
applyPattern v (IdentifierPattern i) = M.singleton (show i) v
applyPattern t p = applyPattern' (value t) p
  where
    applyPattern' :: Value -> Pattern -> M.Map String TypedValue
    applyPattern' v (ConsPattern a b) = case v of
      ListValue (a' : b') -> do
        let headPattern = applyPattern a' a
        let tail' = inferTypes $ ListValue b'
        let tailPattern = applyPattern tail' b
        headPattern `M.union` tailPattern
      ListValue [] -> error "List is empty"
      _ -> error "Not a list"
    applyPattern' (ListValue l) (ListPattern pats) = M.unions $ zipWith applyPattern l pats
    applyPattern' v pat = error $ "Can't apply pattern " ++ show pat ++ " to value " ++ show v

newtype Environment = Environment {bindings :: IORef (M.Map String TypedValue)}

primitiveFunctions :: IO (M.Map String TypedValue)
primitiveFunctions = do
  emptyEnv <- emptyEnvironment
  return $
    M.map inferTypes $
      M.fromList
        [ ("println", FunctionValue emptyEnv (IdentifierPattern $ SimpleIdentifier "value") println),
          ("+", FunctionValue emptyEnv (IdentifierPattern $ OperatorIdentifier "+") (elaraIntInfix (+))),
          ("-", FunctionValue emptyEnv (IdentifierPattern $ OperatorIdentifier "-") (elaraIntInfix (-))),
          ("*", FunctionValue emptyEnv (IdentifierPattern $ OperatorIdentifier "*") (elaraIntInfix (*))),
          ("==", FunctionValue emptyEnv (IdentifierPattern $ OperatorIdentifier "==") elaraEq),
          ("currentTime", FunctionValue emptyEnv (IdentifierPattern $ SimpleIdentifier "currentTime") elaraCurrentTime)
        ]

elaraCurrentTime :: ElaraExecute TypedValue
elaraCurrentTime (TypedValue UnitValue UnitType) _ = do
  nowNanos <- (\time -> round ((fromIntegral time :: Double) / 1000.0)) <$> getCPUTime
  return $ Just $ TypedValue (IntValue nowNanos) intType

elaraIntInfix :: (Integer -> Integer -> Integer) -> ElaraExecute TypedValue
elaraIntInfix f (TypedValue (IntValue a) (NamedType "Int")) s = do
  return $
    Just $
      flip TypedValue (PureFunctionType intType intType) $
        FunctionValue s (IdentifierPattern $ SimpleIdentifier "b") $ \right _ -> do
          let (IntValue b) = value right
          let result = f a b
          return $ Just $ inferTypes $ IntValue result
elaraIntInfix _ a _ = error $ "Can't add " ++ show a

println :: ElaraExecute TypedValue
println tv _ = do
  print $ value tv
  return $ Just $ inferTypes UnitValue

elaraEq :: ElaraExecute TypedValue
elaraEq left s = do
  return $
    Just $
      inferTypes $
        FunctionValue s (IdentifierPattern $ SimpleIdentifier "b") $ \right _ -> do
          let result = left == right
          return $ Just $inferTypes $ BoolValue result

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

data TypedValue = TypedValue {value :: Value, type_ :: Type} deriving (Eq)

instance Show TypedValue where
  show (TypedValue v t) = show v ++ " : " ++ show t

data Value
  = IntValue Integer
  | StringValue String
  | ListValue [TypedValue]
  | UnitValue
  | BoolValue Bool
  | FunctionValue Environment Pattern (ElaraExecute TypedValue)

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

intType :: Type
intType = NamedType "Int"

stringType :: Type
stringType = NamedType "String"

inferTypes :: Value -> TypedValue
inferTypes UnitValue = TypedValue UnitValue UnitType
inferTypes i@(IntValue _) = addType intType i
inferTypes i@(StringValue _) = addType stringType i
inferTypes i@(ListValue []) = addType (ListType (TypeVariable "a")) i
inferTypes i@(ListValue l) = addType (ListType (type_ (head l))) i
inferTypes f@FunctionValue {} = addType (PureFunctionType (TypeVariable "idk") (TypeVariable "idk")) f
inferTypes b@(BoolValue _) = addType (NamedType "Bool") b
inferTypes v = error $ "Can't infer type of " ++ show v

addType :: Type -> Value -> TypedValue
addType t v = TypedValue v t

typeOnlyValue :: Type -> TypedValue
typeOnlyValue = TypedValue (error "Value does not have a type")
