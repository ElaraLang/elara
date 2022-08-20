{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Elara.Data.Module.Inspection where

import Control.Lens (view, (^.))
import Data.List (find)
import Data.Map ((!?))
import Data.Map qualified as M
import Elara.AST.Generic (PatternLike)
import Elara.Data.Module
import Elara.Data.Name hiding (moduleName)
import Elara.Data.Name qualified as Name
import Elara.Data.Uniqueness
import Elara.Error (DesugarError (AmbiguousReference, UnknownVarName))

exposes ::
  MapLike (Structure uniqueness) f =>
  Name ->
  Module expr pattern annotation qualified uniqueness ->
  Bool
exposes name (Module _ _ exposing declarations) =
  case exposing of
    ExposingAll -> isJust (toMap declarations !? name)
    ExposingSome expositions ->
      isJust $
        find
          ( \case
              ExposedValue name' -> name == name'
              ExposedType name' -> name == name'
              ExposedTypeAndAllConstructors name' -> name == name'
          )
          expositions

-- Looks in the import .. as list for any modules imported under the given alias, returning the actual module name if it exists
findAlias ::
  Module expr pattern annotation qualified uniqueness ->
  ModuleName ->
  Maybe ModuleName
findAlias module' name = _importImporting <$> find impNameMatches (module' ^. imports)
  where
    impNameMatches :: Import -> Bool
    impNameMatches imp = imp ^. as == Just name

findModuleOfVar ::
  (PatternLike pattern, MapLike (Structure uniqueness) f) =>
  M.Map ModuleName (Module expr pattern annotation qualified uniqueness) ->
  Module expr pattern annotation qualified uniqueness ->
  Name ->
  Either DesugarError ModuleName
findModuleOfVar modules thisModule varName = do
  -- Firstly look for the name defined in this module, i.e bar = ... ; bar
  let definedInModule = unqualifiedInThisModule thisModule varName
  -- Then look for unqualified name in any imported module, if it's not defined in this module, i.e import Foo ; bar
  let unqualifiedInImported = unqualifiedInImportedModules modules thisModule varName
  -- Then look for qualified name in any imported module, if it's not unqualified, i.e import Foo ; Foo.bar
  let qualifiedInImported = qualifiedInImportedModules modules thisModule varName
  -- Finally look for a qualified name with an alias, i.e import Foo as F ; F.bar
  let qualifiedWithAlias = qualifiedWithAliasModule modules thisModule varName

  case return <$> definedInModule <|> unqualifiedInImported <|> qualifiedInImported <|> qualifiedWithAlias of
    Just x -> x
    Nothing -> Left $ UnknownVarName varName (thisModule ^. name)

unqualifiedInThisModule ::
  (PatternLike pattern, MapLike (Structure uniqueness) f) =>
  Module expr pattern annotation qualified uniqueness ->
  Name ->
  Maybe ModuleName
unqualifiedInThisModule thisModule varName =
  case Name.moduleName varName of
    Just qual ->
      if qual == thisModule ^. name
        then Just (thisModule ^. name)
        else Nothing
    Nothing ->
      thisModule ^. name <$ M.lookup varName (toMap $ thisModule ^. declarations)

unqualifiedInImportedModules ::
  MapLike (Structure uniqueness) f =>
  M.Map ModuleName (Module expr pattern annotation qualified uniqueness) ->
  Module expr pattern annotation qualified uniqueness ->
  Name ->
  Maybe (Either DesugarError ModuleName)
unqualifiedInImportedModules _ _ (Qualified _) = Nothing
unqualifiedInImportedModules modules thisModule varName = do
  let acceptableImports = filter (\imp -> maybe False (exposes varName) (modules M.!? (imp ^. importing))) (thisModule ^. imports)
  case acceptableImports of
    [] -> Nothing
    [import'] -> Just (return $ import' ^. importing)
    multiple -> Just (Left (AmbiguousReference varName (thisModule ^. name) (view importing <$> multiple)))

qualifiedInImportedModules ::
  MapLike (Structure uniqueness) f =>
  M.Map ModuleName (Module expr pattern annotation qualified uniqueness) ->
  Module expr pattern annotation qualified uniqueness ->
  Name ->
  Maybe (Either DesugarError ModuleName)
qualifiedInImportedModules _ _ (Name _) = Nothing
qualifiedInImportedModules modules _ (Qualified (QualifiedName qualification varName)) = do
  let referencedModule = modules M.!? qualification
  case referencedModule of
    Nothing -> Nothing
    Just referencedModule' ->
      if exposes varName referencedModule'
        then Just (return qualification)
        else Nothing

-- Finds `Foo` given `F.bar` and `import Foo as F ; F.bar`
qualifiedWithAliasModule ::
  MapLike (Structure uniqueness) f =>
  M.Map ModuleName (Module expr pattern annotation qualified uniqueness) ->
  Module expr pattern annotation qualified uniqueness ->
  Name ->
  Maybe (Either DesugarError ModuleName)
qualifiedWithAliasModule modules thisModule varName = do
  let actual = Name.moduleName varName >>= findAlias thisModule
  qualifiedInImportedModules modules thisModule (Name.withModule actual varName)