{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.AST.Module.Inspection where

import Control.Lens (Context, (^.), view)
import Data.Map qualified as M
import Elara.AST.Module (Declaration (Declaration), Declaration'(..), Exposing (ExposingAll, ExposingSome), Exposition (ExposedOp, ExposedType, ExposedTypeAndAllConstructors, ExposedValue), HasAs (..), HasDeclarations (declarations), HasExposing (exposing), HasImports (imports), HasName (..), Import (Import), Module (..), importing, qualified, _Declaration)
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), NameLike (fullNameText), OpName, TypeName, VarName (NormalVarName, OperatorVarName))
import Elara.AST.Select (FullASTQual, ASTQual, RUnlocate (..))
import Elara.Error (ReportableError (report))
import Elara.Error.Codes qualified as Codes
import Error.Diagnose
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Reader
import Polysemy.State
import Elara.AST.Region (unlocate, _Unlocate, Located (..))
import Prelude hiding (Reader, State, ask, modify, runState)

data InspectionError ast
  = -- | The name is not defined in any known module
    UnknownName (FullASTQual ast Name)
  | -- | The name is defined in multiple modules
    AmbiguousName (FullASTQual ast Name) (NonEmpty ModuleName)
  
data ContextBuildingError ast
  = -- | Importing an unknown module
    UnknownImportModule (Import ( ast))
  | -- | Importing an unknown element from a known module
    UnknownImportElement (Import ( ast)) (FullASTQual ast Name)
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
  AllNames Eq ast =>
  Module ast ->
  FullASTQual ast Name ->
  Bool
isDeclaring m name' = isJust (find (declares name') ( m ^. declarations <&> view (_Declaration . _Unlocate)))
 where
  declares :: (FullASTQual ast Name) -> Declaration' ast -> Bool
  declares name'' (Declaration' _ name''' _) = name'' == name''' ^. _Unlocate

{- | Returns whether or not a given name is exposed by a given module.
 This does not consider whether or not the name is actually declared in the module.
-}
isExposing ::
  forall ast.
  (AllNames Eq ast, RUnlocate (FullASTQual ast Name) (ASTQual ast Name)) =>
  Module ast ->
  FullASTQual ast Name ->
  Bool
isExposing m name' = case m ^. exposing of
  ExposingAll -> True
  ExposingSome expos -> isJust (find (isExposing' name') expos)
 where
  isExposing' :: FullASTQual ast Name  -> Exposition ast -> Bool
  isExposing' (NVarName q) (ExposedValue name'') = rUnlocate q == rUnlocate name''
  isExposing' (NTypeName q) (ExposedType name'') = rUnlocate q == rUnlocate name''
  isExposing' (NTypeName q) (ExposedTypeAndAllConstructors name'') = rUnlocate q == rUnlocate name''
  isExposing' _ _ = False

importAliasFor :: Import qual -> Located ModuleName
importAliasFor import' = fromMaybe (import' ^. importing) (import' ^. as)

{- | Check if a module is importing another module, returning the import if so, otherwise Nothing.
  This function considers a module to be importing itself, for use in [buildContext]
-}
importFor :: forall ast. Module ast -> Module ast -> Maybe (Import ast)
importFor x y | x ^. name . _Unlocate == y ^. name . _Unlocate = Just (Import (x ^. name) Nothing False ExposingAll)
importFor this imported =
  find (isImporting' (imported ^. name . _Unlocate)) (this ^. imports)
 where
  isImporting' :: ModuleName -> Import (ASTQual ast) -> Bool
  isImporting' name' (Import name'' _ _ _) = name' == (name'' ^. _Unlocate)

type InspectionState ast = M.Map ModuleName (Module ast)

type InspectionContext = M.Map (MaybeQualified Name) (NonEmpty ModuleName)

-- | Hacky function that turns OpVarName into OpName in order to keep all operators in the context in the same form
normalizeName :: MaybeQualified Name -> MaybeQualified Name
normalizeName (NVarName (MaybeQualified n@(NormalVarName _) q)) = NVarName (MaybeQualified n q)
normalizeName (NVarName (MaybeQualified (OperatorVarName opn) q)) = NOpName (MaybeQualified opn q)
normalizeName other = other

search ::
  ( ASTQual ast ~ MaybeQualified
  , Members [Reader InspectionContext, Error (InspectionError ast)] r
  ) =>
  -- | The name to search for
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
  (Member (Error (ContextBuildingError ast)) r, ASTQual ast ~ MaybeQualified) =>
  -- | The module to build the context in relation to
  Module ast ->
  -- | All the known modules. The @thisModule@ does not need to be included, but can be (it will be ignored)
  InspectionState ast ->
  Sem r InspectionContext
buildContext thisModule s = do
  verifyImports thisModule s
  buildContext' (M.insert (thisModule ^. name . _Unlocate) thisModule s)
 where
  buildContext' :: InspectionState ast -> Sem r InspectionContext
  buildContext' inspectionState = fst <$> runState M.empty (traverse_ addDecls (M.elems inspectionState))

  addDecls :: Member (State InspectionContext) r0 => Module ast -> Sem r0 ()
  addDecls m =
    let mImport = thisModule `importFor` m
     in whenJust mImport $ \import' -> do
          traverse_ (undefined (unlocate $ importAliasFor import') (import' ^. qualified)) (m ^. declarations)

  -- add :: forall r0. Member (State InspectionContext) r0 => ModuleName -> Bool -> Declaration ast -> Sem r0 ()
  -- add moduleAlias onlyQualified (Declaration (Located _ (Declaration' actualModule declarationName _))) = do
  --   -- insert the qualified AND unqualified name into the context
  --   let qualify :: MaybeQualified a -> MaybeQualified a
  --       qualify (MaybeQualified x _) = MaybeQualified x (Just moduleAlias)
  --       unqualify :: MaybeQualified a -> MaybeQualified a
  --       unqualify (MaybeQualified x _) = MaybeQualified x Nothing

  --       addQualAndUnqual :: (MaybeQualified a -> MaybeQualified Name) -> MaybeQualified a -> Sem r0 ()
  --       addQualAndUnqual ctor qual = do
  --         modify (M.insertWith (<>) (ctor (qualify qual)) (pure actualModule))
  --         unless onlyQualified (modify (M.insertWith (<>) (ctor (unqualify qual)) (pure actualModule)))

  --   case normalizeName (declarationName ^. _Unlocate) of
  --     NVarName qual -> addQualAndUnqual NVarName qual
  --     NTypeName qual -> addQualAndUnqual NTypeName qual
  --     NOpName op -> addQualAndUnqual NOpName op

-- | Verify that all the imports in a module are valid, i.e. that they are importing a module that exists, and that they are exposing the correct things
verifyImports ::
  forall ast r.
  ( Member (Error (ContextBuildingError ast)) r
  , AllNames Eq ast
  ) =>
  Module ast ->
  InspectionState ast ->
  Sem r ()
verifyImports thisModule inspectionState = do
  traverse_ verifyImport (thisModule ^. imports)
 where
  verifyImport :: Import ast -> Sem r ()
  verifyImport import' = do
    case M.lookup (import' ^. importing . _Unlocate) inspectionState of
      Nothing -> throw (UnknownImportModule import')
      Just importedModule -> do
        let exposed = import' ^. exposing
        case exposed of
          ExposingAll -> pass
          ExposingSome expositions -> traverse_ (verifyExposition import' importedModule) expositions

verifyExposition ::
  forall ast r.
  ( AllNames Eq ast
  , Member (Error (ContextBuildingError ast)) r
  , RUnlocate (FullASTQual ast Name) (ASTQual ast Name)
  ) =>
  Import (ast) ->
  Module ast ->
  Exposition (ast) ->
  Sem r ()
verifyExposition imp m e = verifyExists m e *> verifyExposed m e
 where
  verifyExists :: Module ast -> Exposition ast -> Sem r ()
  verifyExists m' (ExposedValue vn) = verifyExists' m' (overRUnlocate NVarName vn)
  verifyExists m' (ExposedOp vn) = verifyExists' m' (overRUnlocate NOpName vn)
  verifyExists m' (ExposedType tn) = verifyExists' m' (overRUnlocate NTypeName tn)
  verifyExists m' (ExposedTypeAndAllConstructors tn) = verifyExists' m' (overRUnlocate NTypeName tn)

  verifyExists' :: Module ast -> FullASTQual ast Name -> Sem r ()
  verifyExists' m' n = unless (isDeclaring m' n) (throw (UnknownImportElement imp n))

  verifyExposed :: Module ast -> Exposition ast -> Sem r ()
  verifyExposed m' (ExposedValue vn) = verifyExposed' m' (overRUnlocate NVarName vn)
  verifyExposed m' (ExposedOp vn) = verifyExposed' m' (overRUnlocate NOpName vn)
  verifyExposed m' (ExposedType tn) = verifyExposed' m' (overRUnlocate NTypeName tn)
  verifyExposed m' (ExposedTypeAndAllConstructors op) = verifyExposed' m' (overRUnlocate NTypeName op)

  verifyExposed' :: Module ast -> FullASTQual ast Name -> Sem r ()
  verifyExposed' m' n = unless (isExposing m' n) (throw (ExpositionNotPublic imp n))