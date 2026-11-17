-- | Running emitted JVM output
module Elara.JVM.Run (JVMRunError (..), runJVMOutput) where

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.Exception (throwIO, try)
import Effectful.Process (Process, createProcess, proc, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (isDoesNotExistError)

import Elara.Error

import Elara.Error.Codes qualified as Codes

data JVMRunError = JavaNotFound
    deriving (Show)

instance Exception JVMRunError

instance ElaraDiagnostic JVMRunError where
    diagnosticMessage JavaNotFound = "Cannot run the program: java was not found"
    diagnosticCode JavaNotFound = Just Codes.javaNotFound
    diagnosticNotes JavaNotFound = [Hint "Install Java 8 or newer and make sure java is on your PATH"]

-- | Run the @Main@ class in the given directory, exiting with the program's exit code if it fails
runJVMOutput :: (Process :> es, Error ElaraError :> es, HasCallStack) => FilePath -> Eff es ()
runJVMOutput outputDir = do
    res <-
        try $ do
            (_, _, _, process) <- createProcess (proc "java" ["-cp", outputDir, "Main"])
            waitForProcess process
    case res of
        Left err
            | isDoesNotExistError err -> throwElaraError JavaNotFound
            | otherwise -> throwIO err
        Right ExitSuccess -> pass
        Right failure -> throwIO failure
