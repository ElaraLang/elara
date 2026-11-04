{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time embedding of source files into the compiler
module Elara.Data.Embed (embedDir, readDirRecursive) where

import Language.Haskell.TH (Exp, Q, listE, runIO, tupE)
import Language.Haskell.TH.Syntax (addDependentFile, liftString)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (makeRelative, (</>))

{- | Embed every file under a directory (relative to the package root) as @[(FilePath, Text)]@,
with paths relative to that directory.
For example $(embedDir "./") might create something like [("a.txt", "hello"), ("b.txt", "aaa")].

Todo: This might not pick up new files without a clean build! Should probably fix this
-}
embedDir :: FilePath -> Q Exp
embedDir dir = do
    entries <- runIO (readDirRecursive dir)
    for_ entries $ \(path, _) -> runIO (makeAbsolute (dir </> path)) >>= addDependentFile
    listE [tupE [liftString path, [|toText $(liftString (toString contents))|]] | (path, contents) <- entries]

-- | Returns file under a directory, as paths relative to it and their contents, in a stable (but arbitrary) order
readDirRecursive :: FilePath -> IO [(FilePath, Text)]
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
                    contents <- decodeUtf8 <$> readFileBS path
                    pure [(makeRelative root path, contents)]
