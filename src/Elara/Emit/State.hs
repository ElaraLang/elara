module Elara.Emit.State where

import Data.Map qualified as Map
import Elara.Data.Unique (Unique)
import JVM.Data.Raw.Types
import Polysemy (Member, Sem)
import Polysemy.State

data MethodCreationState = MethodCreationState
    { localVariables :: Map LVKey U1
    , maxLocalVariables :: U1
    }
    deriving (Show)

data LVKey
    = UnknownName Int
    | KnownName (Unique Text)
    deriving (Eq, Show, Ord)

initialMethodCreationState :: MethodCreationState
initialMethodCreationState = MethodCreationState Map.empty 0

createMethodCreationState :: Int -> MethodCreationState
createMethodCreationState args =
    MethodCreationState
        (Map.fromList $ zip (UnknownName <$> [0 .. args]) [0 ..])
        (fromIntegral args)

findLocalVariable :: Member (State MethodCreationState) r => Unique Text -> Sem r U1
findLocalVariable v = do
    s <- get
    let lvs = s.localVariables
    case Map.lookup (KnownName v) lvs of
        Just x -> pure x
        Nothing -> do
            let new = maxLocalVariables s
            let newLvs = Map.insert (KnownName v) new lvs
            put $ MethodCreationState{localVariables = newLvs, maxLocalVariables = fromIntegral (length newLvs)}
            pure new

withLocalVariableScope :: Member (State MethodCreationState) r => Sem r a -> Sem r a
withLocalVariableScope act = do
    cur <- get
    x <- act
    modify $ \s -> cur{maxLocalVariables = max (maxLocalVariables cur) (maxLocalVariables s)}
    pure x
