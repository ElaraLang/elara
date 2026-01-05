-- | This module mostly just exists to be a circuit breaker :)
module Elara.Rename.Imports where

import Data.Generics.Product
import Data.Generics.Wrapped
import Elara.AST.Desugared ()
import Elara.AST.Module
import Elara.AST.Name (MaybeQualified (..), ModuleName, Name (..), Qualified (..))
import Elara.AST.Region
import Elara.AST.Select
import Elara.AST.VarRef

{- | Tests that m imports n
This is determined by 2 conditions:
n's module name is in the import list of m
n is exposed by the import
-}
isImportedBy :: Module Desugared -> VarRef Name -> Bool
isImportedBy _ (Local _) = True -- we always assume a local variable exists, even if it doesn't. this condition is checked elsewhere
isImportedBy m (Global (Located _ (Qualified n' nameMod))) = do
    (nameMod == m ^. _Unwrapped % unlocated % field' @"name" % unlocated) || isImportedBy' m n' nameMod
  where
    isImportedBy' :: Module Desugared -> Name -> ModuleName -> Bool
    isImportedBy' m n' nameMod = do
        let mn = m ^. _Unwrapped % unlocated % field' @"name" % unlocated
        let imports' = m ^. _Unwrapped % unlocated % field' @"imports"
        case findImport nameMod imports' of
            Nothing -> False
            Just imp -> do
                let exposing' = imp ^. _Unwrapped % unlocated % field' @"exposing"
                case exposing' of
                    ExposingAll -> True
                    ExposingSome es -> any (isExposition mn n') es
    findImport :: ModuleName -> [Import Desugared] -> Maybe (Import Desugared)
    findImport mn' = find ((mn' ==) . view (_Unwrapped % unlocated % field' @"importing" % unlocated))

    isExposition :: ModuleName -> Name -> Exposition Desugared -> Bool
    isExposition mn (NVarName vn) (ExposedValue vn') = MaybeQualified vn (Just mn) == vn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition mn (NTypeName tn) (ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition _ _ _ = False
