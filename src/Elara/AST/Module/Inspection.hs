{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.AST.Module.Inspection where

import Control.Lens (makePrisms, over, view, (^.))
import Data.Map qualified as M
import Data.Text (intercalate)
import Elara.AST.Module (
    Exposing (ExposingAll, ExposingSome),
    Exposition (ExposedOp, ExposedType, ExposedTypeAndAllConstructors, ExposedValue),
    HasAs (..),
    HasDeclarations (declarations),
    HasExposing (exposing),
    HasImports (imports),
    Import,
    Import' (..),
    Module (..),
    Module',
    importing,
    qualified,
    _Import,
    _Module,
 )
import Elara.AST.Name (MaybeQualified (MaybeQualified), ModuleName, Name (..), NameLike (fullNameText), OpName, TypeName, VarName (OperatorVarName))
import Elara.AST.Region (sourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.AST.Select (ASTDeclaration, ASTLocate, ASTQual, Frontend, FullASTQual, GetLocation (getLocation'), HasDeclarationName (..), HasModuleName (..), HasName (name), RUnlocate (..), rUnlocateVia')
import Elara.Error (ReportableError (report))
import Elara.Error.Codes qualified as Codes
import Elara.Error.Effect (writeReport)
import Error.Diagnose
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer (Writer, runWriterAssocR, tell)
import Prelude hiding (Reader, State, ask, intercalate, intersperse, modify, runState)

data ContextBuildingError ast
    = -- | Importing an unknown module
      UnknownImportModule (Import ast)
    | -- | Importing an unknown element from a known module
      UnknownImportElement (Import ast) (FullASTQual ast Name)
    | -- | The exposition of an import is not exposed by the module
      ExpositionNotPublic (Import ast) (FullASTQual ast Name)

data ContextCleaningError ast
    = -- | The name is defined in multiple modules
      AmbiguousName (MaybeQualified Name) (NonEmpty (ModuleName, ASTLocate ast (MaybeQualified Name)))

type InspectionState ast = M.Map ModuleName (Module ast)
newtype DirtyInspectionContext ast = DirtyInspectionContext (M.Map (MaybeQualified Name) (NonEmpty (ModuleName, ASTLocate ast (MaybeQualified Name))))
makePrisms ''DirtyInspectionContext
newtype CleanedInspectionContext ast = CleanedInspectionContext (M.Map (MaybeQualified Name) (ModuleName, ASTLocate ast (MaybeQualified Name)))
makePrisms ''CleanedInspectionContext

instance GetLocation ast => ReportableError (ContextCleaningError ast) where
    report (AmbiguousName un modules) = do
        let locatedMentions = traverse (bitraverse Just (getLocation' @ast @(MaybeQualified Name))) modules

        let positions = case locatedMentions of
                Nothing -> []
                Just (x :| xs) ->
                    let toPos = sourceRegionToDiagnosePosition . snd
                     in (toPos x, This "defined here") : fmap ((,Where "also defined here") . toPos) xs

        writeReport $
            Err
                (Just Codes.ambiguousName)
                ("Ambiguous name: " <> fullNameText un)
                positions
                [ Note $ "The name is defined in multiple modules: " <> intercalate ", " (fullNameText . fst <$> toList modules)
                , Hint $ "Try to qualify the name with the module name, e.g. " <> fullNameText (fst (head modules)) <> "." <> fullNameText un
                ]

instance (ast ~ Frontend, RUnlocate ast) => ReportableError (ContextBuildingError ast) where
    report (UnknownImportModule un) = do
        let position = sourceRegionToDiagnosePosition (un ^. (_Import . unlocated . importing . sourceRegion))
        writeReport $
            Err
                (Just Codes.unknownModule)
                ("Unknown module: " <> fullNameText (un ^. importing))
                [(position, This "imported here")]
                [Hint "Did you type the module name incorrectly or forget to install a package?", Note "You might have trouble with the latter as packages don't exist yet hehe"]
    report _ = error "Not implemented"

type AllNames :: (Type -> Constraint) -> Type -> Constraint
type AllNames c ast = (c (ASTQual ast VarName), c (ASTQual ast TypeName), c (ASTQual ast OpName))

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

deriving instance
    ( Show (ASTLocate ast (MaybeQualified Name))
    ) =>
    Show (ContextCleaningError ast)

deriving instance
    ( Eq (ASTLocate ast (MaybeQualified Name))
    ) =>
    Eq (ContextCleaningError ast)

deriving instance
    ( Ord (ASTLocate ast (MaybeQualified Name))
    ) =>
    Ord (ContextCleaningError ast)

-- | Returns whether or not a given name is declared in a given module
isDeclaring ::
    forall ast.
    (RUnlocate ast, Eq (ASTQual ast Name), HasDeclarationName (ASTDeclaration ast) ast) =>
    Module ast ->
    FullASTQual ast Name ->
    Bool
isDeclaring m name' = isJust (find (declares (rUnlocate @ast name')) (m ^. declarations))
  where
    declares :: ASTQual ast Name -> ASTDeclaration ast -> Bool
    declares name'' decl = name'' == view (unlocatedDeclarationName @(ASTDeclaration ast) @ast) decl

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

importAliasFor :: RUnlocate ast => Import ast -> ASTLocate ast ModuleName
importAliasFor import' = fromMaybe (import' ^. importing) (import' ^. as)

-- | Check if a module is importing another module, returning the import if so, otherwise Nothing.
importFor :: forall ast. RUnlocate ast => Module ast -> Module ast -> Maybe (Import ast)
importFor this imported =
    find (isImporting' (rUnlocate' @ast (imported ^. name)) . rUnlocateVia' @ast _Import) (this ^. imports)
  where
    isImporting' :: ModuleName -> Import' ast -> Bool
    isImporting' name' (Import' name'' _ _ _) = name' == rUnlocate' @ast name''

-- | Hacky function that turns OpVarName into OpName in order to keep all operators in the context in the same form
normalizeName :: Name -> Name
normalizeName ((NVarName (OperatorVarName opn))) = NOpName opn
normalizeName other = other

{- | Verifies that a given context is valid, i.e. that there are no ambiguous names
| We could do this in the context building, but then we wouldn't be able to report all the errors at once, and couldn't report where all the locations of a name are
-}
verifyContext ::
    forall ast r.
    ( Members
        [ Reader (DirtyInspectionContext ast)
        , Error (NonEmpty (ContextCleaningError ast))
        ]
        r
    , Ord (ASTLocate ast (MaybeQualified Name))
    ) =>
    Sem r (CleanedInspectionContext ast)
verifyContext = do
    context <- view _DirtyInspectionContext <$> ask
    (errors, cleaned) <- runWriterAssocR $ do
        M.traverseMaybeWithKey verifyName context
    case toList errors of
        [] -> pure (CleanedInspectionContext cleaned)
        x : xs -> throw (x :| xs)
  where
    verifyName :: MaybeQualified Name -> NonEmpty (ModuleName, ASTLocate ast (MaybeQualified Name)) -> Sem (Writer (Set (ContextCleaningError ast)) ': r) (Maybe (ModuleName, ASTLocate ast (MaybeQualified Name)))
    verifyName _ (x :| []) = pure $ Just x
    verifyName mn l = do
        tell [AmbiguousName mn l]
        pure Nothing

search ::
    forall ast r.
    ( Member (Reader (CleanedInspectionContext ast)) r
    ) =>
    MaybeQualified Name ->
    Sem r (Maybe ModuleName)
search (normalizeName -> normalizedName) = do
    context <- view _CleanedInspectionContext <$> ask
    pure $ fst <$> M.lookup normalizedName context

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
    , Eq (Module' ast)
    , HasModuleName (ASTDeclaration ast) ast
    , HasDeclarationName (ASTDeclaration ast) ast
    ) =>
    -- | The module to build the context in relation to
    Module ast ->
    -- | All the known modules. The @thisModule@ does not need to be included, but can be (it will be ignored)
    InspectionState ast ->
    Sem r (DirtyInspectionContext ast)
buildContext thisModule s = do
    verifyImports thisModule s
    buildContext' (M.insert (rUnlocate' @ast (thisModule ^. name)) thisModule s)
  where
    buildContext' :: InspectionState ast -> Sem r (DirtyInspectionContext ast)
    buildContext' inspectionState = fst <$> runState (DirtyInspectionContext M.empty) (traverse_ addDecls (M.elems inspectionState))

    addDecls :: Member (State (DirtyInspectionContext ast)) r0 => Module ast -> Sem r0 ()
    addDecls m
        | unlocateModule thisModule == unlocateModule m =
            traverse_ (add (rUnlocate' @ast (m ^. name)) False) (m ^. declarations)
      where
        unlocateModule = rUnlocateVia' @ast _Module :: Module ast -> Module' ast
    addDecls m =
        let mImport = thisModule `importFor` m
         in whenJust mImport $ \import' -> do
                traverse_ (add (rUnlocate' @ast (importAliasFor import')) (import' ^. qualified)) (m ^. declarations)

    add ::
        forall r0.
        (Member (State (DirtyInspectionContext ast)) r0) =>
        ModuleName ->
        Bool ->
        ASTDeclaration ast ->
        Sem r0 ()
    add moduleAlias onlyQualified declaration = do
        let actualModule :: ModuleName
            actualModule = view (unlocatedModuleName @(ASTDeclaration ast) @ast) declaration

            qualify :: MaybeQualified a -> MaybeQualified a
            qualify (MaybeQualified x _) = MaybeQualified x (Just moduleAlias)

            unqualify :: MaybeQualified a -> MaybeQualified a
            unqualify (MaybeQualified x _) = MaybeQualified x Nothing

            -- insert the qualified AND unqualified name into the context
            addQualAndUnqual :: ASTLocate ast (MaybeQualified Name) -> Sem r0 ()
            addQualAndUnqual qual = do
                let unlocatedName = rUnlocate' @ast qual :: MaybeQualified Name
                let modNamePair = (actualModule, qual)
                let modifyDE = modify . over _DirtyInspectionContext
                modifyDE (M.insertWith (<>) (qualify unlocatedName) (pure modNamePair))

                unless
                    onlyQualified
                    (modifyDE (M.insertWith (<>) (unqualify unlocatedName) (pure modNamePair)))

            normalizedName :: ASTLocate ast Name
            normalizedName = fmapRUnlocate' @ast normalizeName $ view (declarationName @(ASTDeclaration ast) @ast) declaration
        addQualAndUnqual normalizedName

-- | Verify that all the imports in a module are valid, i.e. that they are importing a module that exists, and that they are exposing the correct things
verifyImports ::
    forall ast r.
    ( Member (Error (ContextBuildingError ast)) r
    , RUnlocate ast
    , ASTQual ast ~ MaybeQualified
    , HasDeclarationName (ASTDeclaration ast) ast
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
    , HasDeclarationName (ASTDeclaration ast) ast
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
