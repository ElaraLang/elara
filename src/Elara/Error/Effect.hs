{-# LANGUAGE TemplateHaskell #-}

module Elara.Error.Effect where

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.State.Static.Local
import Effectful.TH (makeEffect)
import Error.Diagnose.Diagnostic (Diagnostic)
import Error.Diagnose.Diagnostic qualified as Diagnostic (addFile, addReport)
import Error.Diagnose.Report (Report)

{- | Essentially a very specialised Writer effect for Diagnostics, but safer
| Because the Semigroup instance for Diagnostics is a little funky, there's a chance of accidentally overwriting the
| entire FileMap, so this effect is designed to prevent that, and also prevent the state / writer boilerplate as an added bonus :)
-}
data DiagnosticWriter t :: Effect where
    WriteDiagnostic :: Diagnostic t -> DiagnosticWriter t m ()
    WriteReport :: Report t -> DiagnosticWriter t m ()
    AddFile :: FilePath -> String -> DiagnosticWriter t m ()

type instance DispatchOf (DiagnosticWriter t) = 'Dynamic

makeEffect ''DiagnosticWriter

execDiagnosticWriter :: Eff (DiagnosticWriter t ': r) a -> Eff r (Diagnostic t)
execDiagnosticWriter = fmap fst . runDiagnosticWriter

evalDiagnosticWriter :: Eff (DiagnosticWriter t ': r) a -> Eff r a
evalDiagnosticWriter = fmap snd . runDiagnosticWriter

runDiagnosticWriter :: Eff (DiagnosticWriter t ': r) a -> Eff r (Diagnostic t, a)
runDiagnosticWriter = reinterpret (fmap swap . runState mempty) $ \_ -> \case
    WriteDiagnostic d -> modify (<> d)
    WriteReport r -> modify (`Diagnostic.addReport` r)
    AddFile fp s -> modify (\x -> Diagnostic.addFile x fp s)
