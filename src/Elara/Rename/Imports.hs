-- | This module mostly just exists to be a circuit breaker :)
module Elara.Rename.Imports (isImportedBy) where

import Elara.AST.Name (MaybeQualified (..), ModuleName, Name (..), Qualified (..))
import Elara.AST.New.Module
import Elara.AST.New.Phases.Desugared qualified as NewD
import Elara.AST.Region
import Elara.AST.VarRef

{- | Tests that m imports n
This is determined by 2 conditions:
n's module name is in the import list of m
n is exposed by the import
-}
isImportedBy :: Module SourceRegion NewD.Desugared -> VarRef Name -> Bool
isImportedBy _ (Local _) = True -- we always assume a local variable exists, even if it doesn't. this condition is checked elsewhere
isImportedBy (Module _ m) (Global (Located _ (Qualified n' nameMod))) = do
    (nameMod == m.moduleName ^. unlocated) || isImportedBy' m n' nameMod
  where
    isImportedBy' :: Module' SourceRegion NewD.Desugared -> Name -> ModuleName -> Bool
    isImportedBy' m' n nameMod = do
        let imports' = m'.moduleImports
        case findImport nameMod imports' of
            Nothing -> False
            Just (Import _ imp) -> do
                case imp.importExposing of
                    ExposingAll -> True
                    ExposingSome es -> any (isExposition (m'.moduleName ^. unlocated) n) es
    findImport :: ModuleName -> [Import SourceRegion NewD.Desugared] -> Maybe (Import SourceRegion NewD.Desugared)
    findImport mn' = find (\(Import _ imp) -> mn' == imp.importModuleName ^. unlocated)

    isExposition :: ModuleName -> Name -> Exposition SourceRegion NewD.Desugared -> Bool
    isExposition mn (NVarName vn) (ExposedValue vn') = MaybeQualified vn (Just mn) == vn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition _ _ _ = False
