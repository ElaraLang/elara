module Elara.JVM.Emit.State where

import Data.Map qualified as Map
import Effectful
import Effectful.State.Static.Local
import Elara.Data.Pretty
import Elara.Data.Unique
import JVM.Data.Abstract.Builder.Code (CodeBuilder, newLabel)
import JVM.Data.Abstract.Builder.Label
import JVM.Data.Abstract.Name (QualifiedClassName)
import JVM.Data.Abstract.Type qualified as JVM
import JVM.Data.Raw.Types (U1, U2)

data MethodCreationState = MethodCreationState
    { localVariables :: !(Map LVKey U2)
    , maxLocalVariables :: !U2
    , thisClassName :: QualifiedClassName
    -- ^ The name of the class this method belongs to
    , labels :: Map (Unique Text) Label
    -- ^ Map from IR block labels to JVM labels
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
initialMethodCreationState name = MethodCreationState Map.empty 0 name Map.empty

createMethodCreationState :: [Unique Text] -> QualifiedClassName -> MethodCreationState
createMethodCreationState args thisName =
    MethodCreationState
        (Map.fromList $ zip (KnownName <$> args) [0 ..])
        (fromIntegral $ length args)
        thisName
        mempty

findLocalVariable :: State MethodCreationState :> r => Unique Text -> Eff r U2
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
  where
    isWide (JVM.PrimitiveFieldType JVM.Long) = True
    isWide (JVM.PrimitiveFieldType JVM.Double) = True
    isWide _ = False

getLabel :: (State MethodCreationState :> r, CodeBuilder :> r) => Unique Text -> Eff r Label
getLabel labelName = do
    emitState <- get
    case Map.lookup labelName emitState.labels of
        Just lbl -> pure lbl
        Nothing -> do
            label <- newLabel
            put emitState{labels = Map.insert labelName label emitState.labels}
            pure label
