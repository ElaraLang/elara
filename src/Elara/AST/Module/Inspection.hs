{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.AST.Module.Inspection where

import Control.Lens (view, (^.))
import Data.Map qualified as M
import Elara.AST.Module (
  Declaration (Declaration),
  Declaration' (..),
  Exposing (ExposingAll, ExposingSome),
  Exposition (ExposedOp, ExposedType, ExposedTypeAndAllConstructors, ExposedValue),
  HasAs (..),
  HasDeclarations (declarations),
  HasExposing (exposing),
  HasImports (imports),
  HasName (..),
  Import,
  Import' (..),
  Module (..),
  importing,
  qualified,
  _Declaration,
  _Import,
 )
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), NameLike (fullNameText), OpName, TypeName, VarName (OperatorVarName))
import Elara.AST.Region (Located (..), _Unlocate)
import Elara.AST.Select (ASTLocate, ASTQual, FullASTQual, RUnlocate (..))
import Elara.Error (ReportableError (report))
import Elara.Error.Codes qualified as Codes
import Error.Diagnose
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Reader
import Polysemy.State
import Prelude hiding (Reader, State, ask, modify, runState)

data InspectionError ast
  = -- | The name is not defined in any known module
    UnknownName (MaybeQualified Name)
  | -- | The name is defined in multiple modules
    AmbiguousName (MaybeQualified Name) (NonEmpty ModuleName)

data ContextBuildingError ast
  = -- | Importing an unknown module
    UnknownImportModule (Import ast)
  | -- | Importing an unknown element from a known module
    UnknownImportElement (Import ast) (FullASTQual ast Name)
  | -- | The exposition of an import is not exposed by the module
    ExpositionNotPublic (Import ast) (FullASTQual ast Name)

instance (NameLike (FullASTQual ast VarName), NameLike (FullASTQual ast TypeName), NameLike (ASTQual ast OpName)) => ReportableError (InspectionError ast) where
  report (UnknownName un) = Err (Just Codes.unknownName) ("Unknown name: " <> fullNameText un) [] []
  -- report (UnknownImportModule un) = Err (Just Codes.unknownModule) ("Unknown module: " <> fullNameText (un ^. importing)) [] []
  report _ = error "Not implemented"

type AllNames :: (Type -> Constraint) -> Type -> Constraint
type AllNames c ast = (c (ASTQual ast VarName), c (ASTQual ast TypeName), c (ASTQual ast OpName))

deriving instance
  ( Show (FullASTQual ast Name)
  , Show (Import ast)
  , AllNames Show ast
  ) =>
  Show (InspectionError ast)

deriving instance
  ( Eq (FullASTQual ast Name)
  , Eq (Import ast)
  , AllNames Eq ast
  ) =>
  Eq (InspectionError ast)

deriving instance
  ( Show (FullASTQual ast Name)
  , Show (Import ast)
  , AllNames Show ast
  ) =>
  Show (ContextBuildingError ast)

deriving instance
  ( Eq (FullASTQual ast Name)
  , Eq (Import ast)
  , AllNames Eq ast
  ) =>
  Eq (ContextBuildingError ast)

-- | Returns whether or not a given name is declared in a given module
isDeclaring ::
  forall ast.
  (RUnlocate ast, Eq (ASTQual ast Name)) =>
  Module ast ->
  FullASTQual ast Name ->
  Bool
isDeclaring m name' = isJust (find (declares (rUnlocate @ast name')) (m ^. declarations <&> view (_Declaration . _Unlocate)))
 where
  declares :: ASTQual ast Name -> Declaration' ast -> Bool
  declares name'' (Declaration' _ name''' _) = name'' == rUnlocate @ast name'''

{- | Returns whether or not a given name is exposed by a given module.
 This does not consider whether or not the name is actually declared in the module.
-}
isExposing ::
  forall ast.
  (RUnlocate ast, ASTQual ast ~ MaybeQualified) =>
  Module ast ->
  FullASTQual ast Name ->
  Bool
isExposing m name' = case m ^. exposing of
  ExposingAll -> True
  ExposingSome expos -> isJust (find (isExposing' (rUnlocate @ast name')) expos)
 where
  isExposing' :: MaybeQualified Name -> Exposition ast -> Bool
  isExposing' mq@(MaybeQualified (NVarName vn) _) (ExposedValue name'') = (vn <$ mq) == rUnlocate @ast name''
  isExposing' mq@(MaybeQualified (NTypeName tn) _) (ExposedType name'') = (tn <$ mq) == rUnlocate @ast name''
  isExposing' mq@(MaybeQualified (NTypeName tn) _) (ExposedTypeAndAllConstructors name'') = (tn <$ mq) == rUnlocate @ast name''
  isExposing' _ _ = False

importAliasFor :: Import ast -> ASTLocate ast ModuleName
importAliasFor import' = fromMaybe (import' ^. importing) (import' ^. as)

-- | Check if a module is importing another module, returning the import if so, otherwise Nothing.
importFor :: forall ast. RUnlocate ast => Module ast -> Module ast -> Maybe (Import ast)
importFor this imported =
  find (isImporting' (rUnlocate' @ast (imported ^. name)) . view (_Import . _Unlocate)) (this ^. imports)
 where
  isImporting' :: ModuleName -> Import' ast -> Bool
  isImporting' name' (Import' name'' _ _ _) = name' == rUnlocate' @ast name''

type InspectionState ast = M.Map ModuleName (Module ast)

type InspectionContext = M.Map (MaybeQualified Name) (NonEmpty ModuleName)

-- | Hacky function that turns OpVarName into OpName in order to keep all operators in the context in the same form
normalizeName :: MaybeQualified Name -> MaybeQualified Name
normalizeName (MaybeQualified (NVarName (OperatorVarName opn)) q) = MaybeQualified (NOpName opn) q
normalizeName other = other

search ::
  ( Members [Reader InspectionContext, Error (InspectionError ast)] r
  ) =>
  MaybeQualified Name ->
  Sem r ModuleName
search (normalizeName -> normalizedName) = do
  context <- ask
  case M.lookup normalizedName context of
    Nothing -> throw (UnknownName normalizedName)
    Just (x :| []) -> pure x
    Just possibles -> throw (AmbiguousName normalizedName possibles)

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
  ( Member (Error (ContextBuildingError ast)) r
  , ASTQual ast ~ MaybeQualified
  , RUnlocate ast
  ) =>
  -- | The module to build the context in relation to
  Module ast ->
  -- | All the known modules. The @thisModule@ does not need to be included, but can be (it will be ignored)
  InspectionState ast ->
  Sem r InspectionContext
buildContext thisModule s = do
  verifyImports thisModule s
  buildContext' (M.insert (rUnlocate' @ast (thisModule ^. name)) thisModule s)
 where
  buildContext' :: InspectionState ast -> Sem r InspectionContext
  buildContext' inspectionState = fst <$> runState M.empty (traverse_ addDecls (M.elems inspectionState))

  addDecls :: Member (State InspectionContext) r0 => Module ast -> Sem r0 ()
  -- addDecls m | thisModule == m = error "TODO" TODO DONT FORGET
  addDecls m =
    let mImport = thisModule `importFor` m
     in whenJust mImport $ \import' -> do
          traverse_ (add (rUnlocate' @ast (importAliasFor import')) (import' ^. qualified)) (m ^. declarations)

  add :: forall r0. Member (State InspectionContext) r0 => ModuleName -> Bool -> Declaration ast -> Sem r0 ()
  add moduleAlias onlyQualified (Declaration (Located _ (Declaration' actualModule declarationName _))) = do
    -- insert the qualified AND unqualified name into the context
    let qualify :: MaybeQualified a -> MaybeQualified a
        qualify (MaybeQualified x _) = MaybeQualified x (Just moduleAlias)
        unqualify :: MaybeQualified a -> MaybeQualified a
        unqualify (MaybeQualified x _) = MaybeQualified x Nothing

        addQualAndUnqual :: MaybeQualified Name -> Sem r0 ()
        addQualAndUnqual qual = do
          modify (M.insertWith (<>) (qualify qual) (pure (rUnlocate' @ast actualModule)))
          unless
            onlyQualified
            (modify (M.insertWith (<>) (unqualify qual) (pure (rUnlocate' @ast actualModule))))

        normalizedName :: MaybeQualified Name
        normalizedName = normalizeName (rUnlocate @ast declarationName)
    addQualAndUnqual normalizedName

-- | Verify that all the imports in a module are valid, i.e. that they are importing a module that exists, and that they are exposing the correct things
verifyImports ::
  forall ast r.
  ( Member (Error (ContextBuildingError ast)) r
  , RUnlocate ast
  , ASTQual ast ~ MaybeQualified
  ) =>
  Module ast ->
  InspectionState ast ->
  Sem r ()
verifyImports thisModule inspectionState = do
  traverse_ verifyImport (thisModule ^. imports)
 where
  verifyImport :: Import ast -> Sem r ()
  verifyImport import' = do
    case M.lookup (rUnlocate' @ast (import' ^. importing)) inspectionState of
      Nothing -> throw (UnknownImportModule import')
      Just importedModule -> do
        let exposed = import' ^. exposing
        case exposed of
          ExposingAll -> pass
          ExposingSome expositions -> traverse_ (verifyExposition import' importedModule) expositions

verifyExposition ::
  forall ast r.
  ( Member (Error (ContextBuildingError ast)) r
  , RUnlocate ast
  , ASTQual ast ~ MaybeQualified
  ) =>
  Import ast ->
  Module ast ->
  Exposition ast ->
  Sem r ()
verifyExposition imp m e = verifyExists m e *> verifyExposed m e
 where
  verifyExists :: Module ast -> Exposition ast -> Sem r ()
  verifyExists m' (ExposedValue vn) = verifyExists' m' (fmapRUnlocate @ast NVarName vn)
  verifyExists m' (ExposedOp vn) = verifyExists' m' (fmapRUnlocate @ast NOpName vn)
  verifyExists m' (ExposedType tn) = verifyExists' m' (fmapRUnlocate @ast NTypeName tn)
  verifyExists m' (ExposedTypeAndAllConstructors tn) = verifyExists' m' (fmapRUnlocate @ast NTypeName tn)

  verifyExists' :: Module ast -> FullASTQual ast Name -> Sem r ()
  verifyExists' m' n = unless (isDeclaring m' n) (throw (UnknownImportElement imp n))

  verifyExposed :: Module ast -> Exposition ast -> Sem r ()
  verifyExposed m' (ExposedValue vn) = verifyExposed' m' (fmapRUnlocate @ast NVarName vn)
  verifyExposed m' (ExposedOp vn) = verifyExposed' m' (fmapRUnlocate @ast NOpName vn)
  verifyExposed m' (ExposedType tn) = verifyExposed' m' (fmapRUnlocate @ast NTypeName tn)
  verifyExposed m' (ExposedTypeAndAllConstructors op) = verifyExposed' m' (fmapRUnlocate @ast NTypeName op)

  verifyExposed' :: Module ast -> FullASTQual ast Name -> Sem r ()
  verifyExposed' m' n = unless (isExposing m' n) (throw (ExpositionNotPublic imp n))