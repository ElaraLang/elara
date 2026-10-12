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
import H2JVM
import H2JVM.Name

import Effectful.Error.Extra (fromEither)
import Elara.AST.Name (ModuleName)
import Elara.Data.Pretty (Pretty (..), viaShow)
import Elara.Error (ElaraDiagnostic (..), runErrorAsElaraError)
import Elara.JVM.Error (JVMLoweringError)
import Elara.JVM.Lower (lowerModule)
import Elara.Query (Query (..))
import Elara.Query.Effects (QueryEffects)

import Elara.JVM.Emit qualified as Emit
import Elara.JVM.IR qualified as IR
import Rock qualified

-- | Orphan instance, TODO: sort this out by improving h2jvm
instance Pretty CodeConverterError where
    pretty = viaShow

instance Exception CodeConverterError

instance ElaraDiagnostic CodeConverterError where
    diagnosticMessage = pretty

-- | Lower a Core module to JVM IR
runGetJVMIRModuleQuery ::
    (QueryEffects es, Rock.Rock Query :> es) =>
    ModuleName ->
    Eff es IR.Module
runGetJVMIRModuleQuery mn = runErrorAsElaraError @JVMLoweringError $ do
    coreModule <- Rock.fetch (GetFinalisedCoreModule mn)
    lowerModule coreModule

-- | Emit JVM IR to ClassFiles
runGetJVMClassFilesQuery ::
    (QueryEffects es, Rock.Rock Query :> es) =>
    ModuleName ->
    Eff es [ClassFile]
runGetJVMClassFilesQuery mn = runErrorAsElaraError @JVMLoweringError $ do
    irModule <- Rock.fetch (GetJVMIRModule mn)
    Emit.emitIRModule irModule

-- | Serialize ClassFiles to bytes with file paths
runGetJVMClassBytesQuery ::
    (QueryEffects es, Rock.Rock Query :> es) =>
    ModuleName ->
    Eff es [(FilePath, LByteString)]
runGetJVMClassBytesQuery mn = runErrorAsElaraError @CodeConverterError $ do
    classFiles <- Rock.fetch (GetJVMClassFiles mn)
    for classFiles $ \cf -> do
        bytes <- fromEither $ classFileBytes cf
        pure (suitableFilePath cf.name, bytes)
