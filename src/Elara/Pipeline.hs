{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Each stage of AST processing can be interpreted as part of an overall pipeline of effects in the 'Sem' monad.
This acts as the entrypoint to the stage, bringing each stage into a common abstraction
-}
module Elara.Pipeline where

import Data.Text.IO qualified as Text
import Elara.Data.Pretty
import Elara.Error (DiagnosticWriter, runDiagnosticWriter)
import Elara.Logging
import Error.Diagnose (Diagnostic)
import Polysemy (Effect, Embed, InterpreterFor, Member, Members, Sem, embed, runM, subsume_)
import Polysemy.Log (DataLog, interpretDataLog, interpretDataLogStdoutWith)
import Polysemy.Maybe (MaybeE, runMaybe)
import Print (elaraDebug, printPretty)
import System.IO (openFile)

-- | All stages of a pipeline must be interpreted into this effect stack.
type PipelineResultEff = '[MaybeE, DiagnosticWriter (Doc AnsiStyle), StructuredDebug, Embed IO]

type IsPipeline r = Members PipelineResultEff r

type PipelineRes a = IO (Diagnostic (Doc AnsiStyle), Maybe a)

type family EffectsAsPrefixOf (effects :: [Effect]) (r :: [Effect]) :: [Effect] where
    EffectsAsPrefixOf '[] ys = ys
    EffectsAsPrefixOf (x ': xs) ys = x ': EffectsAsPrefixOf xs ys

-- -- | Finalise a pipeline, returning the final diagnostic and the result of the pipeline.
-- finalisePipeline :: Sem PipelineResultEff a -> PipelineRes a
-- finalisePipeline =
--     runM @IO
--         . runDiagnosticWriter
--         . runMaybe
--         . (if elaraDebug then logToStdoutAndFile else destroyDataLog)
--         . structuredDebugToLog
--         . subsume_

logToStdoutAndFile :: Member (Embed IO) r => InterpreterFor (DataLog (Doc AnsiStyle)) r
logToStdoutAndFile sem = do
    -- reset log
    embed $ writeFileText "elara.log" ""
    handle <- embed (openFile "elara.log" WriteMode)
    embed $ hSetBuffering handle LineBuffering
    interpretDataLog
        ( \x -> do
            embed $ printPretty x
            embed $ Text.hPutStrLn handle (prettyToUnannotatedText x)
        )
        sem

destroyDataLog :: InterpreterFor (DataLog (Doc AnsiStyle)) r
destroyDataLog = interpretDataLog (const pass)
