{- | JVM Backend query implementations for the Rock query system.

This module implements the queries for JVM lowering, class file emission,
and serialization. Each query is atomic and operates on a single module.
-}
module Elara.JVM.Query (
    runGetJVMIRModuleQuery,
    runGetJVMClassFilesQuery,
    runGetJVMClassBytesQuery,
) where

import Data.Binary.Put (runPut)
import Data.Binary.Write (writeBinary)
import Effectful
import Effectful.Error.Extra (fromEither)
import Effectful.Error.Static (Error)
import Elara.AST.Name (ModuleName)
import Elara.JVM.Emit qualified as Emit
import Elara.JVM.Error (JVMLoweringError)
import Elara.JVM.IR qualified as IR
import Elara.JVM.Lower (lowerModule)
import Elara.Query (Query (..))
import Elara.Query.Effects (ConsQueryEffects)
import JVM.Data.Abstract.ClassFile (ClassFile (..))
import JVM.Data.Abstract.Name (suitableFilePath)
import JVM.Data.Convert (convert)
import JVM.Data.Convert.Monad (CodeConverterError)
import Rock qualified

-- | Lower a Core module to JVM IR
runGetJVMIRModuleQuery ::
    ModuleName ->
    Eff (ConsQueryEffects '[Error JVMLoweringError, Rock.Rock Query]) IR.Module
runGetJVMIRModuleQuery mn = do
    coreModule <- Rock.fetch (GetFinalisedCoreModule mn)
    lowerModule coreModule

-- | Emit JVM IR to ClassFiles
runGetJVMClassFilesQuery ::
    ModuleName ->
    Eff (ConsQueryEffects '[Error JVMLoweringError, Rock.Rock Query]) [ClassFile]
runGetJVMClassFilesQuery mn = do
    irModule <- Rock.fetch (GetJVMIRModule mn)
    Emit.emitIRModule irModule

-- | Serialize ClassFiles to bytes with file paths
runGetJVMClassBytesQuery ::
    ModuleName ->
    Eff (ConsQueryEffects '[Error JVMLoweringError, Error CodeConverterError, Rock.Rock Query]) [(FilePath, LByteString)]
runGetJVMClassBytesQuery mn = do
    classFiles <- Rock.fetch (GetJVMClassFiles mn)
    for classFiles $ \cf -> do
        converted <- fromEither $ convert cf
        let bytes = runPut (writeBinary converted)
        pure (suitableFilePath cf.name, bytes)
