module Elara.Emit.State where

import Data.Map qualified as Map
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type (FieldType)
import JVM.Data.Raw.Types
import Polysemy (Member, Sem)
import Polysemy.State

data MethodCreationState = MethodCreationState
    { localVariables :: !(Map LVKey U1)
    , maxLocalVariables :: !U1
    , thisClassName :: QualifiedClassName
    }
    deriving (Show)

instance Pretty MethodCreationState where
    pretty MethodCreationState{localVariables, maxLocalVariables, thisClassName} =
        vcat
            [ "MethodCreationState"
            , bracedBlock
                [ "localVariables:" <+> pretty localVariables
                , "maxLocalVariables:" <+> pretty maxLocalVariables
                , "thisClassName:" <+> pretty thisClassName
                ]
            ]

data LVKey
    = -- | A method argument that we don't know the name of
      UnknownName !Int
    | -- | A local variable that we do know the name of
      KnownName !(Unique Text)
    deriving (Eq, Show, Ord)

instance Pretty LVKey where
    pretty = \case
        UnknownName i -> "UnknownName" <+> pretty i
        KnownName u -> "KnownName" <+> pretty u

initialMethodCreationState :: QualifiedClassName -> MethodCreationState
initialMethodCreationState = MethodCreationState Map.empty 0

-- | creates a "nested" method creation state, taking the existing state and appending the given arguments
createMethodCreationStateOf :: MethodCreationState -> [Unique Text] -> MethodCreationState
createMethodCreationStateOf copy args =
    let newLvs = copy.localVariables <> Map.fromList (zip (KnownName <$> args) [maxLocalVariables copy ..])
        newMax = maxLocalVariables copy + fromIntegral (length args)
     in copy{localVariables = newLvs, maxLocalVariables = newMax}

createMethodCreationState :: [Unique Text] -> QualifiedClassName -> MethodCreationState
createMethodCreationState args =
    MethodCreationState
        (Map.fromList $ zip (KnownName <$> args) [0 ..])
        (fromIntegral $ length args)

findLocalVariable :: Member (State MethodCreationState) r => Unique Text -> Sem r U1
findLocalVariable v = do
    s <- get
    let lvs = s.localVariables
    case Map.lookup (KnownName v) lvs of
        Just x -> pure x
        Nothing -> do
            let new = maxLocalVariables s
            let newLvs = Map.insert (KnownName v) new lvs
            put $ s{localVariables = newLvs, maxLocalVariables = fromIntegral (length newLvs)}
            pure new

withLocalVariableScope :: Member (State MethodCreationState) r => Sem r a -> Sem r a
withLocalVariableScope act = do
    cur <- get
    x <- act
    modify $ \s -> cur{maxLocalVariables = max (maxLocalVariables cur) (maxLocalVariables s)}
    pure x
