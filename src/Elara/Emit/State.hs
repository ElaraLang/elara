module Elara.Emit.State where

import Data.Map qualified as Map
import Elara.AST.VarRef
import Elara.Core
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import Elara.Emit.Utils (generateFieldType)
import Elara.Emit.Var (JVMBinder (..), jvmLocalTypeToFieldType)
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type (FieldType)
import JVM.Data.Raw.Types
import Polysemy (Member, Sem)
import Polysemy.State
import Print

data MethodCreationState = MethodCreationState
    { localVariables :: !(Map LVKey (U1, FieldType))
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

newtype LVKey
    = -- | A local variable that we do know the name of
      KnownName (Unique Text)
    deriving (Eq, Show, Ord)

instance Pretty LVKey where
    pretty = \case
        KnownName u -> "KnownName" <+> pretty u

initialMethodCreationState :: QualifiedClassName -> MethodCreationState
initialMethodCreationState = MethodCreationState Map.empty 0

-- | creates a "nested" method creation state, taking the existing state and appending the given arguments
createMethodCreationStateOf :: MethodCreationState -> [(Unique Text, FieldType)] -> MethodCreationState
createMethodCreationStateOf copy args =
    let newValues :: [(U1, FieldType)] = zip [maxLocalVariables copy ..] (map snd args)
        newLvs = copy.localVariables <> Map.fromList (zip (KnownName . fst <$> args) newValues)
        newMax = maxLocalVariables copy + fromIntegral (length args)
     in copy{localVariables = newLvs, maxLocalVariables = newMax}

createMethodCreationState :: [(Unique Text, FieldType)] -> QualifiedClassName -> MethodCreationState
createMethodCreationState args =
    MethodCreationState
        (Map.fromList $ zip (KnownName . fst <$> args) (zip [0 ..] (map snd args)))
        (fromIntegral $ length args)

addLocalVariable :: MethodCreationState -> Unique Text -> FieldType -> MethodCreationState
addLocalVariable s v t =
    let new = maxLocalVariables s
        newLvs = Map.insert (KnownName v) (new, t) (localVariables s)
     in s{localVariables = newLvs, maxLocalVariables = new + 1}

lookupVar :: Member (State MethodCreationState) r => Unique Text -> Sem r (U1, FieldType)
lookupVar v = do
    s <- get
    case Map.lookup (KnownName v) (localVariables s) of
        Just x -> pure x
        Nothing -> error $ "lookupVar: variable not found: " <> show v <> " in " <> showPretty s

findLocalVariable :: Member (State MethodCreationState) r => (Unique Text, Type) -> Sem r (U1, FieldType)
findLocalVariable (v, t) = do
    s <- get
    let lvs = s.localVariables
    case Map.lookup (KnownName v) lvs of
        Just x -> pure x
        Nothing -> do
            let new = maxLocalVariables s
            let newLvs = Map.insert (KnownName v) (new, generateFieldType t) lvs
            put $ s{localVariables = newLvs, maxLocalVariables = fromIntegral (length newLvs)}
            pure (new, generateFieldType t)

findLocalVariable' :: Member (State MethodCreationState) r => JVMBinder -> Sem r (U1, Maybe FieldType)
findLocalVariable' = \case
    JVMLocal i t -> pure (i, jvmLocalTypeToFieldType <$> t)
    (Normal ((Id (Local' v) t _))) -> fmap Just <$> findLocalVariable (v, t)
    other -> error $ "findLocalVariable' called with non-local variable: " <> show other

withLocalVariableScope :: Member (State MethodCreationState) r => Sem r a -> Sem r a
withLocalVariableScope act = do
    cur <- get
    x <- act
    modify $ \s -> cur{maxLocalVariables = max (maxLocalVariables cur) (maxLocalVariables s)}
    pure x
