{-# LANGUAGE TemplateHaskell #-}

{- | The Elara standard library bundled into the compiler binary to make a truly portable build.

Unless overridden (e.g. with @--stdlib-dir@), the bundled sources are unpacked into the user's $XDG_CACHE directory and
compiled from there.
-}
module Elara.Stdlib (
    StdlibError (..),
    embeddedStdlib,
    resolveStdlibDir,
    unpackStdlib,
    ejectStdlib,
) where

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.Exception (IOException, throwIO, try)
import Effectful.FileSystem (
    FileSystem,
    XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    getXdgDirectory,
    renameDirectory,
 )
import Effectful.Temporary (Temporary, withTempDirectory)
import GHC.Fingerprint (fingerprintFingerprints, fingerprintString)
import System.FilePath (takeDirectory, (</>))

import Effectful.FileSystem.IO.ByteString qualified as Eff

import Elara.Data.Embed (embedDir)
import Elara.Data.Pretty
import Elara.Error

import Elara.Data.Pretty.Styles qualified as Style
import Elara.Error.Codes qualified as Codes

data StdlibError
    = -- | A stdlib directory was explicitly given, but doesn't exist
      StdlibDirNotFound !FilePath
    | -- | Refusing to eject into an existing path
      EjectTargetExists !FilePath
    deriving (Show)

instance Exception StdlibError

instance ElaraDiagnostic StdlibError where
    diagnosticMessage (StdlibDirNotFound dir) = "Standard library directory not found: " <> Style.bold (pretty dir)
    diagnosticMessage (EjectTargetExists dir) = "Cannot eject the standard library: " <> Style.bold (pretty dir) <> " already exists"
    diagnosticCode (StdlibDirNotFound _) = Just Codes.stdlibDirNotFound
    diagnosticCode (EjectTargetExists _) = Just Codes.ejectTargetExists
    diagnosticNotes (StdlibDirNotFound dir) = [Hint $ "Create one with " <> Style.bold ("elara stdlib eject " <> pretty dir)]
    diagnosticNotes (EjectTargetExists _) = [Hint "Choose a different directory, or remove the existing one first"]

-- | The standard library sources, as paths relative to the stdlib root and their contents
embeddedStdlib :: [(FilePath, ByteString)]
embeddedStdlib = $(embedDir "stdlib")

-- | Determine the stdlib directory to compile against: the custom one if present, otherwise the bundled stdlib
resolveStdlibDir ::
    (FileSystem :> es, Temporary :> es, Error ElaraError :> es) =>
    Maybe FilePath ->
    Eff es FilePath
resolveStdlibDir = \case
    Nothing -> unpackStdlib
    Just dir -> do
        unlessM (doesDirectoryExist dir) $ throwElaraError (StdlibDirNotFound dir)
        pure dir

-- | Unpack the bundled stdlib into the cache directory, or reuse if already present, and return its path.
unpackStdlib :: (FileSystem :> es, Temporary :> es) => Eff es FilePath
unpackStdlib = do
    cacheDir <- getXdgDirectory XdgCache "elara"
    let dir = cacheDir </> ("stdlib-" <> show contentHash)
    unlessM (doesDirectoryExist dir) $ do
        createDirectoryIfMissing True cacheDir
        -- use temp directory so concurrent things don't break
        withTempDirectory cacheDir "stdlib.tmp" $ \tmp -> do
            writeStdlib tmp
            renamed <- try @IOException (renameDirectory tmp dir)
            -- if the rename failed because another process got there first, use its copy
            whenLeft_ renamed $ \err -> unlessM (doesDirectoryExist dir) (throwIO err)
    pure dir
  where
    -- note the use of show rather than e.g. decodeUtf8 here
    -- the actual content that we include in the hash is fairly arbitrary, and it's better than risking decode failures
    contentHash = fingerprintFingerprints [fingerprintString (path <> "\0" <> show contents) | (path, contents) <- embeddedStdlib]

-- | Write a copy of the bundled stdlib to the passed directory
ejectStdlib :: (FileSystem :> es, Error ElaraError :> es, HasCallStack) => FilePath -> Eff es ()
ejectStdlib dir = do
    whenM (doesPathExist dir) $ throwElaraError (EjectTargetExists dir)
    writeStdlib dir

writeStdlib :: FileSystem :> es => FilePath -> Eff es ()
writeStdlib dir = for_ embeddedStdlib $ \(path, contents) -> do
    let file = dir </> path
    createDirectoryIfMissing True (takeDirectory file)
    Eff.writeFile file contents
