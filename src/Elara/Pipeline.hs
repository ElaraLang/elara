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
import Polysemy (Effect, Embed, Members, Sem, runM, subsume_, interpret, InterpreterFor, raise_)
import Polysemy.Log (Log, Severity (..), interpretLogStdoutLevel, DataLog, interpretDataLog, interpretDataLogStdoutWith)
import Polysemy.Maybe (MaybeE, runMaybe)
import Polysemy.Time.Interpreter.Ghc
import Print (elaraDebug)
import Elara.Logging

-- | All stages of a pipeline must be interpreted into this effect stack.
type PipelineResultEff = '[MaybeE, DiagnosticWriter (Doc AnsiStyle), StructuredDebug, Embed IO]

type IsPipeline r = Members PipelineResultEff r

type PipelineRes a = IO (Diagnostic (Doc AnsiStyle), Maybe a)

type family EffectsAsPrefixOf (effects :: [Effect]) (r :: [Effect]) :: [Effect] where
    EffectsAsPrefixOf '[] ys = ys
    EffectsAsPrefixOf (x ': xs) ys = x ': EffectsAsPrefixOf xs ys

-- | Finalise a pipeline, returning the final diagnostic and the result of the pipeline.
finalisePipeline :: Sem PipelineResultEff a -> PipelineRes a
finalisePipeline =
    runM @IO
        . runDiagnosticWriter
        . runMaybe
        . (if elaraDebug then interpretDataLogStdoutWith prettyToText else destroyDataLog)
        . structuredDebugToLog
        . subsume_

destroyDataLog :: InterpreterFor (DataLog (Doc AnsiStyle)) r
destroyDataLog = interpretDataLog (\_ -> pass)