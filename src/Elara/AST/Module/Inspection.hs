{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Module.Inspection where

import Control.Lens ((^.))
import Data.Map qualified as M
import Elara.AST.Module (Declaration (Declaration), Exposing (ExposingAll, ExposingSome), Exposition (ExposedType, ExposedTypeAndAllConstructors, ExposedValue), HasAs (..), HasDeclarations (declarations), HasExposing (exposing), HasImports (imports), HasName (..), Import (Import), Module (..), importing, qualified)
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), OpName, TypeName, VarName)
import Elara.AST.Select (ASTQual)
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Reader
import Polysemy.State
import Prelude hiding (Reader, State, ask, modify, runState)

data InspectionError ast
  = -- | The name is not defined in any known module
    UnknownName (Name (ASTQual ast))
  | -- | Importing an unknown module
    UnknownImportModule (Import (ASTQual ast))
  | -- | Importing an unknown element from a known module
    UnknownImportElement (Import (ASTQual ast)) (Name (ASTQual ast))
  | -- | The name is defined in multiple modules
    AmbiguousName (Name (ASTQual ast)) (NonEmpty ModuleName)
  | -- | The exposition of an import does not exist in the module
    ExpositionDoesNotExist (Exposition (ASTQual ast))
  | -- | The exposition of an import is not exposed by the module
    ExpositionNotPublic (Import (ASTQual ast)) (Name (ASTQual ast))

type AllNames :: (Type -> Constraint) -> Type -> Constraint
type AllNames c ast = (c (ASTQual ast VarName), c (ASTQual ast TypeName), c (ASTQual ast OpName))

deriving instance
  ( Show (Name (ASTQual ast))
  , Show (Import (ASTQual ast))
  , AllNames Show ast
  ) =>
  Show (InspectionError ast)

deriving instance
  ( Eq (Name (ASTQual ast))
  , Eq (Import (ASTQual ast))
  , AllNames Eq ast
  ) =>
  Eq (InspectionError ast)

-- | Returns whether or not a given name is declared in a given module
isDeclaring ::
  forall ast.
  AllNames Eq ast =>
  Module ast ->
  Name (ASTQual ast) ->
  Bool
isDeclaring (Module _ _ _ decls) name' = isJust (find (declares name') decls)
 where
  declares :: Name (ASTQual ast) -> Declaration ast -> Bool
  declares name'' (Declaration _ name''' _) = name'' == name'''

{- | Returns whether or not a given name is exposed by a given module.
 This does not consider whether or not the name is actually declared in the module.
-}
isExposing ::
  forall ast.
  AllNames Eq ast =>
  Module ast ->
  Name (ASTQual ast) ->
  Bool
isExposing m name' = case m ^. exposing of
  ExposingAll -> True
  ExposingSome expos -> isJust (find (isExposing' name') expos)
 where
  isExposing' :: Name (ASTQual ast) -> Exposition (ASTQual ast) -> Bool
  isExposing' (NVarName q) (ExposedValue name'') = q == name''
  isExposing' (NTypeName q) (ExposedType name'') = q == name''
  isExposing' (NTypeName q) (ExposedTypeAndAllConstructors name'') = q == name''
  isExposing' _ _ = False

importAliasFor :: Import qual -> ModuleName
importAliasFor import' = fromMaybe (import' ^. importing) (import' ^. as)

{- | Check if a module is importing another module, returning the import if so, otherwise Nothing.
  This function considers a module to be importing itself, for use in [buildContext]
-}
importFor :: forall ast. Module ast -> Module ast -> Maybe (Import (ASTQual ast))
importFor x y | x ^. name == y ^. name = Just (Import (x ^. name) Nothing False ExposingAll)
importFor this imported =
  find (isImporting' (imported ^. name)) (this ^. imports)
 where
  isImporting' :: ModuleName -> Import (ASTQual ast) -> Bool
  isImporting' name' (Import name'' _ _ _) = name' == name''

type InspectionState ast = M.Map ModuleName (Module ast)

type InspectionContext = M.Map (Name MaybeQualified) (NonEmpty ModuleName)

search ::
  ( ASTQual ast ~ MaybeQualified
  , Members [Reader InspectionContext, Error (InspectionError ast)] r
  ) =>
  -- | The name to search for
  Name MaybeQualified ->
  Sem r ModuleName
search elementName = do
  context <- ask
  case M.lookup elementName context of
    Nothing -> throw (UnknownName elementName)
    Just (x :| []) -> pure x
    Just possibles -> throw (AmbiguousName elementName possibles)

{- | There are a lot of cases to consider when looking for a module:

        1. The name is defined in this module, i.e @let bar = ... ; bar@. It may or may not be qualified
        2. The name is unqualified in an imported module, i.e @import Foo (bar) ; bar@
        3. The name is qualified in an imported module, i.e @import Foo (bar) ; Foo.bar@
        4. The name is qualified with an alias in an imported module, i.e @import Foo as F (bar) ; F.bar@

        Step 1 is more or less the same as step 2, if we treat every module as if it imports itself.

        Similarly, step 3 is the same as step 4, just with a different name for the module.

        This can be simplified to a single step where we build a "context" of all the imported modules, each with an optional qualified name, then
        we look for the name in that context.

        For example, if we have the following imports:

        >    module Quux exposing (quux)
        >    ...
        >    module This exposing (bar)
        >    import Bar (bar)
        >    import Foo (fooBar)
        >    import Baz as B (baz, Baz)
        >    import Quux qualified

        the context would be

        >    { (bar, [This, Bar])
        >    , (This.bar, [This])
        >    , (fooBar, [Foo])
        >    , Foo.fooBar -> [Foo]
        >    , baz -> [Baz]
        >    , B.baz -> [Baz]
        >    , Baz -> [Baz]
        >    , quux -> [Quux]
        >    , Quux.quux -> [Quux]
        >    }
-}
buildContext ::
  forall ast r.
  (Member (Error (InspectionError ast)) r, ASTQual ast ~ MaybeQualified) =>
  -- | The module to build the context in relation to
  Module ast ->
  -- | All the known modules. The @thisModule@ does not need to be included, but can be (it will be ignored)
  InspectionState ast ->
  Sem r InspectionContext
buildContext thisModule s = do
  verifyImports thisModule s
  buildContext' (M.insert (thisModule ^. name) thisModule s)
 where
  buildContext' :: InspectionState ast -> Sem r InspectionContext
  buildContext' inspectionState = fst <$> runState M.empty (traverse_ addDecls (M.elems inspectionState))

  addDecls :: Member (State InspectionContext) r0 => Module ast -> Sem r0 ()
  addDecls m =
    let mImport = thisModule `importFor` m
     in whenJust mImport $ \import' -> do
          traverse_ (add (importAliasFor import') (import' ^. qualified)) (m ^. declarations)

  add :: forall r0. Member (State InspectionContext) r0 => ModuleName -> Bool -> Declaration ast -> Sem r0 ()
  add moduleAlias onlyQualified (Declaration actualModule declarationName _) = do
    -- insert the qualified AND unqualified name into the context
    let qualify :: MaybeQualified a -> MaybeQualified a
        qualify (MaybeQualified x _) = MaybeQualified x (Just moduleAlias)
        unqualify :: MaybeQualified a -> MaybeQualified a
        unqualify (MaybeQualified x _) = MaybeQualified x Nothing

        addQualAndUnqual :: (MaybeQualified a -> Name MaybeQualified) -> MaybeQualified a -> Sem r0 ()
        addQualAndUnqual ctor qual = do
          modify (M.insertWith (<>) (ctor (qualify qual)) (pure actualModule))
          unless onlyQualified (modify (M.insertWith (<>) (ctor (unqualify qual)) (pure actualModule)))

    case declarationName of
      NVarName qual -> addQualAndUnqual NVarName qual
      NTypeName qual -> addQualAndUnqual NTypeName qual
      NOpName op -> addQualAndUnqual NOpName op

-- | Verify that all the imports in a module are valid, i.e. that they are importing a module that exists, and that they are exposing the correct things
verifyImports ::
  forall ast r.
  ( Member (Error (InspectionError ast)) r
  , AllNames Eq ast
  ) =>
  Module ast ->
  InspectionState ast ->
  Sem r ()
verifyImports thisModule inspectionState = do
  traverse_ verifyImport (thisModule ^. imports)
 where
  verifyImport :: Import (ASTQual ast) -> Sem r ()
  verifyImport import' = do
    case M.lookup (import' ^. importing) inspectionState of
      Nothing -> throw (UnknownImportModule import')
      Just importedModule -> do
        let exposed = import' ^. exposing
        case exposed of
          ExposingAll -> pass
          ExposingSome expositions -> traverse_ (verifyExposition import' importedModule) expositions

verifyExposition ::
  ( AllNames Eq ast
  , Member (Error (InspectionError ast)) r
  ) =>
  Import (ASTQual ast) ->
  Module ast ->
  Exposition (ASTQual ast) ->
  Sem r ()
verifyExposition imp m e = verifyExists m e *> verifyExposed m e
 where
  verifyExists m' (ExposedValue vn) = verifyExists' m' (NVarName vn)
  verifyExists m' (ExposedType tn) = verifyExists' m' (NTypeName tn)
  verifyExists m' (ExposedTypeAndAllConstructors op) = verifyExists' m' (NTypeName op)

  verifyExists' m' n = unless (isDeclaring m' n) (throw (UnknownImportElement imp n))

  verifyExposed m' (ExposedValue vn) = verifyExposed' m' (NVarName vn)
  verifyExposed m' (ExposedType tn) = verifyExposed' m' (NTypeName tn)
  verifyExposed m' (ExposedTypeAndAllConstructors op) = verifyExposed' m' (NTypeName op)

  verifyExposed' m' n = unless (isExposing m' n) (throw (ExpositionNotPublic imp n))