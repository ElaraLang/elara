module Elara.ModuleIndex (
    ModuleStyle (..),
    ModuleEntry (..),
    ModuleIndex (..),
    buildModuleIndex,
    lookupModule,
    lookupFile,
)
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Effectful (Eff)
import Effectful.FileSystem (FileSystem)
import Elara.AST.Name (ModuleName (..))
import Elara.Data.Pretty
import Elara.ReadFile (findElaraFiles)
import Elara.Settings (CompilerSettings (..))
import System.FilePath (takeBaseName, takeDirectory, takeFileName, (</>))

-- | The style of module path resolution
data ModuleStyle
    = -- | Nested: A/B/C.elr → A.B.C
      NestedStyle
    | -- | Rust-style: A/B/C/mod.elr → A.B.C
      RustStyle
    | -- | Flat: A.B.C.elr → A.B.C
      FlatStyle
    deriving (Show, Eq, Ord, Generic)

instance Pretty ModuleStyle

-- | An entry in the module index
data ModuleEntry = ModuleEntry
    { entryPath :: !FilePath
    , entryStyle :: !ModuleStyle
    }
    deriving (Show, Eq, Ord, Generic)

instance Pretty ModuleEntry

{- | Bidirectional mapping between module names and file paths.
Note: 'moduleToFiles' uses lowercase module names as keys
for case-insensitive lookup compatibility with case-insensitive filesystems
-}
data ModuleIndex = ModuleIndex
    { moduleToFiles :: !(Map ModuleName [ModuleEntry])
    -- ^ Map from normalised module name to all candidate files (list for conflict detection)
    , fileToModule :: !(Map FilePath ModuleName)
    -- ^ Map from file path to inferred module name (original case)
    }
    deriving (Show, Eq, Generic)

instance Pretty ModuleIndex

instance Semigroup ModuleIndex where
    ModuleIndex m1 f1 <> ModuleIndex m2 f2 =
        ModuleIndex
            (Map.unionWith (<>) m1 m2)
            (Map.union f1 f2)

instance Monoid ModuleIndex where
    mempty = ModuleIndex Map.empty Map.empty

-- | Normalize a module name to lowercase for case-insensitive comparison
normalizeModuleName :: ModuleName -> ModuleName
normalizeModuleName (ModuleName parts) = ModuleName (fmap Text.toLower parts)

-- | Build the module index by scanning all source directories
buildModuleIndex :: CompilerSettings -> Eff '[FileSystem] ModuleIndex
buildModuleIndex settings = do
    let mainFileDir = case settings.mainFile of
            Just fp -> [] -- TODO: if we have a
            Nothing -> []
    let roots =
            ordNub $
                ["stdlib"]
                    <> settings.sourceDirs
                    <> mainFileDir

    allFiles <- findElaraFiles roots
    pure $ foldMap (indexFile roots) allFiles

-- | Index a single file, inferring its module name from the path
indexFile :: [FilePath] -> FilePath -> ModuleIndex
indexFile roots filePath = case inferModuleName roots filePath of
    Nothing -> mempty
    Just (moduleName, style) ->
        let entry = ModuleEntry{entryPath = filePath, entryStyle = style}
            normalizedName = normalizeModuleName moduleName
         in ModuleIndex
                { moduleToFiles = one (normalizedName, [entry])
                , fileToModule = one (filePath, moduleName)
                }

-- | Infer a module name from a file path relative to source roots
inferModuleName :: [FilePath] -> FilePath -> Maybe (ModuleName, ModuleStyle)
inferModuleName roots filePath =
    -- Try each root and return the first successful inference
    asum [inferFromRoot root filePath | root <- roots]

-- | Infer module name from a file path relative to a specific root
inferFromRoot :: FilePath -> FilePath -> Maybe (ModuleName, ModuleStyle)
inferFromRoot root filePath = do
    -- Check if the file is under this root
    relativePath <- stripPrefix' (root </> "") filePath
    inferFromRelativePath relativePath

-- | Strip a prefix from a string, returning 'Nothing' if the prefix doesn't match
stripPrefix' :: FilePath -> FilePath -> Maybe FilePath
stripPrefix' prefix str
    | prefix `isPrefixOf` str = Just (drop (length prefix) str)
    | otherwise = Nothing

-- | Infer module name from a path relative to a source root
inferFromRelativePath :: FilePath -> Maybe (ModuleName, ModuleStyle)
inferFromRelativePath relativePath
    -- Rust-style: A/B/C/mod.elr → A.B.C
    | takeFileName relativePath == "mod.elr" =
        let dirPath = takeDirectory relativePath
            parts = splitPath' dirPath
         in if null parts
                then Nothing
                else Just (ModuleName (fromList parts), RustStyle)
    -- Flat: A.B.C.elr → A.B.C
    | '.' `elem` takeBaseName relativePath =
        let baseName = takeBaseName relativePath
            parts = Text.splitOn "." (toText baseName)
         in if null parts || any Text.null parts
                then Nothing
                else Just (ModuleName (fromList parts), FlatStyle)
    -- Nested: A/B/C.elr → A.B.C
    | otherwise =
        let dirPath = takeDirectory relativePath
            baseName = takeBaseName relativePath
            dirParts = if dirPath == "." then [] else splitPath' dirPath
            parts = dirParts <> [toText baseName]
         in if null parts
                then Nothing
                else Just (ModuleName (fromList parts), NestedStyle)

-- | Split a path into its components
splitPath' :: FilePath -> [Text]
splitPath' path =
    let components = filter (not . null) $ splitOn '/' path
     in map toText components

-- | Split a string on a character
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s =
    let (before, after) = break (== c) s
     in before : case after of
            [] -> []
            (_ : rest) -> splitOn c rest

-- | Look up all module entries for a given module name (case-insensitive)
lookupModule :: ModuleName -> ModuleIndex -> Maybe [ModuleEntry]
lookupModule mn idx = Map.lookup (normalizeModuleName mn) idx.moduleToFiles

-- | Look up the module name for a given file path
lookupFile :: FilePath -> ModuleIndex -> Maybe ModuleName
lookupFile fp idx = Map.lookup fp idx.fileToModule
