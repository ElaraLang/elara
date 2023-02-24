{-# LANGUAGE TemplateHaskell #-}

module Elara.Error.Effect where

import Error.Diagnose (def)
import Error.Diagnose.Diagnostic as Diagnostic (Diagnostic, addFile, addReport)
import Error.Diagnose.Report (Report)
import Polysemy
import Polysemy.State
import Prelude hiding (modify, modify', runState)

data DiagnosticWriter t m a where
    AddDiagnostic :: Diagnostic t -> DiagnosticWriter t m ()
    AddReport :: Report t -> DiagnosticWriter t m ()
    AddFile :: FilePath -> String -> DiagnosticWriter t m ()

makeSem ''DiagnosticWriter

execDiagnosticWriter :: Sem (DiagnosticWriter t ': r) a -> Sem r (Diagnostic t)
execDiagnosticWriter = fmap fst . runDiagnosticWriter

runDiagnosticWriter :: Sem (DiagnosticWriter t ': r) a -> Sem r (Diagnostic t, a)
runDiagnosticWriter =
    runState def
        . reinterpretH
            ( \case
                AddDiagnostic d -> do
                    modify' (<> d) >>= pureT
                AddReport r -> do
                    modify' (`Diagnostic.addReport` r) >>= pureT
                AddFile fp s -> do
                    modify (\x -> Diagnostic.addFile x fp s) >>= pureT
            )