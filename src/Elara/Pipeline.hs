{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Each stage of AST processing can be interpreted as part of an overall pipeline of effects in the 'Sem' monad.
This acts as the entrypoint to the stage, bringing each stage into a common abstraction
-}
module Elara.Pipeline where

import Elara.Data.Pretty
import Elara.Error (DiagnosticWriter, runDiagnosticWriter)
import Error.Diagnose (Diagnostic)
import Polysemy (Effect, Embed, Members, Sem, raiseUnder, runM)
import Polysemy.Log (Log, Severity (..), interpretLogStdoutLevel)
import Polysemy.Maybe (MaybeE, runMaybe)
import Polysemy.Time.Interpreter.Ghc

-- | All stages of a pipeline must be interpreted into this effect stack.
type PipelineResultEff = '[MaybeE, DiagnosticWriter (Doc AnsiStyle), Log, Embed IO]

type IsPipeline r = Members PipelineResultEff r

type PipelineRes a = IO (Diagnostic (Doc AnsiStyle), Maybe a)

type family EffectsAsPrefixOf (effects :: [Effect]) (r :: [Effect]) :: [Effect] where
    EffectsAsPrefixOf '[] ys = ys
    EffectsAsPrefixOf (x ': xs) ys = x ': EffectsAsPrefixOf xs ys

-- | Finalise a pipeline, returning the final diagnostic and the result of the pipeline.
finalisePipeline :: Sem PipelineResultEff a -> PipelineRes a
finalisePipeline =
    runM @IO
        . interpretTimeGhc
        . interpretLogStdoutLevel (Just Debug)
        . raiseUnder
        . runDiagnosticWriter
        . runMaybe
