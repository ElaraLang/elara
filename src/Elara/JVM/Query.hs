{- | JVM Backend query implementations for the Rock query system.

This module implements the queries for JVM lowering, class file emission,
and serialization. Each query is atomic and operates on a single module.
-}
module Elara.JVM.Query (
    runGetJVMIRModuleQuery,
    runGetJVMClassFilesQuery,
    runGetJVMClassBytesQuery,
) where

import Effectful
import Effectful.Error.Static (Error)
import H2JVM
import H2JVM.Name

import Effectful.Error.Extra (fromEither)
import Elara.AST.Name (ModuleName)
import Elara.JVM.Error (JVMLoweringError)
import Elara.JVM.Lower (lowerModule)
import Elara.Query (Query (..))
import Elara.Query.Effects (ConsQueryEffects)

import Elara.JVM.Emit qualified as Emit
import Elara.JVM.IR qualified as IR
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
        bytes <- fromEither $ classFileBytes cf
        pure (suitableFilePath cf.name, bytes)
