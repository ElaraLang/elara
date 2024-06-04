{-# LANGUAGE TemplateHaskell #-}

module Elara.Logging where

import Data.Text qualified as Text
import Polysemy
import Polysemy.Log qualified as Log
import Polysemy.State (State, evalState, get, put)
import Print (elaraDebug)

data StructuredDebug m a where
    Debug :: Text -> StructuredDebug m ()
    DebugWith :: Text -> m a -> StructuredDebug m a

makeSem ''StructuredDebug

structuredDebugToLog :: forall r a. Member Log.Log r => Sem (StructuredDebug : r) a -> Sem r a
structuredDebugToLog =
    if not elaraDebug
        then raise_ . interpretH (\case Debug _ -> pureT (); DebugWith _ act -> runTSimple act)
        else
            subsume_
                . evalState 0
                . reinterpret2H @StructuredDebug @(State Int) @Log.Log @r
                    ( \case
                        Debug msg -> do
                            depth <- get
                            Log.debug $ Text.replicate depth "│ " <> msg
                            pureT ()
                        DebugWith msg act -> do
                            depth <- get
                            Log.debug $ Text.replicate depth "│ " <> msg
                            put $ depth + 1
                            a <- runTSimple act
                            put depth
                            pure a
                    )
