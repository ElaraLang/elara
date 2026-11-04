-- | Compile-time embedding of files into the compiler
module Elara.Data.Embed (embedDir, embedJavaClasses, readDirRecursive) where

import Effectful (runEff)
import Effectful.Temporary (runTemporary, withSystemTempDirectory)
import H2JVM.JVMVersion
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (makeRelative, takeExtension, (</>))
import System.Process (callProcess)

import Language.Haskell.TH.Syntax qualified as TH

import Elara.JVM.Version (targetJavaVersion)

{- | Embed every file under a directory (relative to the package root) as @[(FilePath, ByteString)]@,
with paths relative to that directory.
For example $(embedDir "./") might create something like [("a.txt", "hello"), ("b.txt", "aaa")].

Todo: This might not pick up new files without a clean build! Should probably fix this
-}
embedDir :: FilePath -> Q Exp
embedDir dir = do
    entries <- runIO (readDirRecursive dir)
    dependOnFiles dir (map fst entries)
    TH.lift entries

{- | Compile the Java sources under a directory with @javac@ and embed the resulting class files as
@[(FilePath, ByteString)]@, with paths relative to the classpath root. Needs @javac@ at build time.
-}
embedJavaClasses :: FilePath -> Q Exp
embedJavaClasses dir = do
    sources <- runIO $ filter ((== ".java") . takeExtension) . map fst <$> readDirRecursive dir
    dependOnFiles dir sources
    classes <- runIO $ runEff $ runTemporary $ withSystemTempDirectory "elara-jvm-stdlib" $ \tmp -> liftIO $ do
        callProcess
            "javac"
            ( [ "--release"
              , show (unwrapMajor (getMajor targetJavaVersion) - 44) -- turn major version (e.g. 52) into release (e.g. 8)
              , "-Xlint:-options" -- silence any old version warning
              , "-d"
              , tmp
              ]
                <> map (dir </>) sources
            )
        readDirRecursive tmp
    TH.lift classes

dependOnFiles :: FilePath -> [FilePath] -> Q ()
dependOnFiles dir paths = for_ paths $ \path ->
    runIO (makeAbsolute (dir </> path)) >>= addDependentFile

-- | Returns file under a directory, as paths relative to it and their contents, in a stable (but arbitrary) order
readDirRecursive :: FilePath -> IO [(FilePath, ByteString)]
readDirRecursive root = go root
  where
    go dir = do
        items <- sort <$> listDirectory dir
        fmap concat $ for items $ \item -> do
            let path = dir </> item
            isDir <- doesDirectoryExist path
            if isDir
                then go path
                else do
                    contents <- readFileBS path
                    pure [(makeRelative root path, contents)]
