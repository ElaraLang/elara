{-# LANGUAGE TemplateHaskell #-}

module Elara.Error.Effect where

import Error.Diagnose (def)
import Error.Diagnose.Diagnostic as Diagnostic (Diagnostic, addFile, addReport)
import Error.Diagnose.Report (Report)
import Polysemy
import Polysemy.State
import Prelude hiding (modify', runState)

{- | Essentially a very specialised Writer effect for Diagnostics, but safer
| Because the Semigroup instance for Diagnostics is a little funky, there's a chance of accidentally overwriting the
| entire FileMap, so this effect is designed to prevent that, and also prevent the state / writer boilerplate as an added bonus :)
-}
data DiagnosticWriter t m a where
    WriteDiagnostic :: Diagnostic t -> DiagnosticWriter t m ()
    WriteReport :: Report t -> DiagnosticWriter t m ()
    AddFile :: FilePath -> String -> DiagnosticWriter t m ()

makeSem ''DiagnosticWriter

execDiagnosticWriter :: Sem (DiagnosticWriter t ': r) a -> Sem r (Diagnostic t)
execDiagnosticWriter = fmap fst . runDiagnosticWriter

runDiagnosticWriter :: Sem (DiagnosticWriter t ': r) a -> Sem r (Diagnostic t, a)
runDiagnosticWriter =
    runState def
        . reinterpretH
            ( \case
                WriteDiagnostic d -> do
                    modify' (<> d) >>= pureT
                WriteReport r -> do
                    modify' (`Diagnostic.addReport` r) >>= pureT
                AddFile fp s -> do
                    modify (\x -> Diagnostic.addFile x fp s) >>= pureT
            )
