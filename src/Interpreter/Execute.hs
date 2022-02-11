module Interpreter.Execute where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Lazy (StateT)
import Data.IORef
import Data.Map ((!))
import qualified Data.Map as M
import Interpreter.State
import Interpreter.Value (Value (..))
import Parse.AST

class Execute a where
  execute :: a -> Environment -> IO (Maybe Value)

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
    res <- func i
    return $ Just res
  execute a _ = error $ "Not implemented for " ++ show a
