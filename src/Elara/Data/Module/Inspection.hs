{-# LANGUAGE BlockArguments #-}

module Elara.Data.Module.Inspection where

import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.List (find)
import Data.Map ((!?))
import Data.Map qualified as M
import Data.Maybe (isJust)
import Elara.AST.Generic (PatternLike, patternNameMatches)
import Elara.Data.Module
import Elara.Data.Name hiding (_moduleName)
import Elara.Data.Name qualified as Name
import Elara.Error (DesugarError (AmbiguousReference, UnknownVarName))

exposes :: Name -> Module expr pattern annotation qualified -> Bool
exposes name (Module _ _ exposing declarations) =
  case exposing of
    ExposingAll -> isJust (declarations !? name)
    ExposingSome expositions ->
      isJust $
        find
          ( \case
              ExposedValue name' -> name == name'
              ExposedType name' -> name == name'
              ExposedTypeAndAllConstructors name' -> name == name'
          )
          expositions

findAlias :: Module expr pattern annotation qualified -> ModuleName -> Maybe ModuleName
findAlias module' name = _moduleName <$> find (\imp -> imp._as == Just name) (module'._imports)

findModuleOfVar ::
  (PatternLike pattern) =>
  M.Map ModuleName (Module expr pattern annotation qualified) ->
  Module expr pattern annotation qualified ->
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
    Nothing -> Left $ UnknownVarName varName thisModule._name

unqualifiedInThisModule :: (PatternLike pattern) => Module expr pattern annotation qualified -> Name -> Maybe ModuleName
unqualifiedInThisModule thisModule varName =
  case Name.moduleName varName of
    Just qual ->
      if qual == thisModule._name
        then Just thisModule._name
        else Nothing
    Nothing ->
      thisModule._name <$ (M.lookup varName thisModule._declarations)

unqualifiedInImportedModules ::
  M.Map ModuleName (Module expr pattern annotation qualified) ->
  Module expr pattern annotation qualified ->
  Name ->
  Maybe (Either DesugarError ModuleName)
unqualifiedInImportedModules _ _ (Qualified _) = Nothing
unqualifiedInImportedModules modules thisModule varName = do
  let acceptableImports = filter (\import' -> maybe False (exposes varName) (modules M.!? import'._moduleName)) thisModule._imports
  case acceptableImports of
    [] -> Nothing
    [import'] -> Just (return import'._moduleName)
    multiple -> Just (Left (AmbiguousReference varName thisModule._name (_moduleName <$> multiple)))

qualifiedInImportedModules ::
  M.Map ModuleName (Module expr pattern annotation qualified) ->
  Module expr pattern annotation qualified ->
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

qualifiedWithAliasModule ::
  M.Map ModuleName (Module expr pattern annotation qualified) ->
  Module expr pattern annotation qualified ->
  Name ->
  Maybe (Either DesugarError ModuleName)
qualifiedWithAliasModule modules thisModule varName = do
  let actual = Name.moduleName varName >>= findAlias thisModule
  qualifiedInImportedModules modules thisModule (Name.withModule actual varName)