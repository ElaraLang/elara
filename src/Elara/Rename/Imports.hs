-- | This module mostly just exists to be a circuit breaker :)
module Elara.Rename.Imports (isImportedBy, isExposition, expositionToLocatedName) where

import Elara.AST.Module
import Elara.AST.Name (MaybeQualified (..), ModuleName, Name (..), Qualified (..), VarName (..))
import Elara.AST.Phases.Desugared qualified as NewD
import Elara.AST.Region
import Elara.AST.VarRef

{- | Tests whether an 'Exposition' entry matches a given 'Name' from a module.

The qualifier in an 'Exposition' entry may be 'Nothing' (as is the case for entries in import
statements, which are unqualified in source) or @'Just' q@ (as is the case for entries in a
module's own @exposing@ list). A 'Nothing' qualifier is treated as a wildcard and matches any
module; a @'Just' q@ qualifier must equal @mn@.
-}
isExposition :: ModuleName -> Name -> Exposition SourceRegion NewD.Desugared -> Bool
isExposition mn (NameOp opn) (ExposedOp (Located _ (MaybeQualified opn' mq))) =
    opn == opn' && maybe True (== mn) mq
isExposition mn (NameValue vn) (ExposedValue (Located _ (MaybeQualified (NormalVarName vn') mq))) =
    vn == vn' && maybe True (== mn) mq
isExposition mn (NameOp opn) (ExposedValue (Located _ (MaybeQualified (OperatorVarName opn') mq))) =
    opn == opn' && maybe True (== mn) mq
isExposition mn (NameType tn) (ExposedType (Located _ (MaybeQualified tn' mq))) =
    tn == tn' && maybe True (== mn) mq
isExposition mn (NameType tn) (ExposedTypeAndAllConstructors (Located _ (MaybeQualified tn' mq))) =
    tn == tn' && maybe True (== mn) mq
isExposition _ _ _ = False

-- | Extract a 'Located Name' from an 'Exposition' for error reporting
expositionToLocatedName :: Exposition SourceRegion NewD.Desugared -> Located Name
expositionToLocatedName (ExposedValue (Located sr (MaybeQualified (NormalVarName vn) _))) = Located sr (NameValue vn)
expositionToLocatedName (ExposedValue (Located sr (MaybeQualified (OperatorVarName opn) _))) = Located sr (NameOp opn)
expositionToLocatedName (ExposedOp (Located sr (MaybeQualified opn _))) = Located sr (NameOp opn)
expositionToLocatedName (ExposedType (Located sr (MaybeQualified tn _))) = Located sr (NameType tn)
expositionToLocatedName (ExposedTypeAndAllConstructors (Located sr (MaybeQualified tn _))) = Located sr (NameType tn)

{- | Tests that @m@ imports @n@
This is determined by 2 conditions:
1. @n@'s module name is in the import list of @m@
2. @n@ is exposed by the import
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
                case imp.importExposingOrHiding of
                    ImportExposing ExposingAll -> True
                    ImportExposing (ExposingSome es) -> any (isExposition (m'.moduleName ^. unlocated) n) es
                    ImportHiding hidings ->
                        not $ any (isExposition (m'.moduleName ^. unlocated) n) hidings
    findImport :: ModuleName -> [Import SourceRegion NewD.Desugared] -> Maybe (Import SourceRegion NewD.Desugared)
    findImport mn' = find (\(Import _ imp) -> mn' == imp.importModuleName ^. unlocated)
