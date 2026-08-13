{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeAbstractions #-}

{- | Renaming stage of compilation
This stage handles:
1. Renaming all variables, types, and type variables, adding module qualification or unique suffixes to avoid name clashes
2. Desugaring any "first-class" pattern matches into normal match expressions (eg '\[] -> 1' to '\x -> match x with [] -> 1')
3. Desugaring blocks into let-in chains (and monad operations soon), eg 'let y = 1; y + 1' to 'let y = 1 in y + 1'
Note that until the monad operations are implemented, we can't fully remove blocks, as we have nothing to translate 'f x; g x' into
-}
module Elara.Rename (renameExpr, InnerRename) where

import Data.Foldable1 (Foldable1 (fold1), foldl1', foldr1)
import Data.Generics.Product hiding (list)
import Effectful (Eff, (:>))
import Effectful.Error.Static (throwError)

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Effectful.Error.Static qualified as Eff
import Effectful.Reader.Static qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import Effectful.State.Static.Local qualified as Local

import Effectful.State.Extra
import Elara.AST.Extensions
import Elara.AST.Location
import Elara.AST.Name (DeclName (..), LowerAlphaName (..), MaybeQualified (MaybeQualified), ModuleName (..), Name (..), Qualified (Qualified), ToName (toName), TypeName (..), VarName (..))
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Desugared (DesugaredExpressionExtension (..))
import Elara.AST.Phases.Renamed (Renamed, RenamedExpressionExtension (..), TypedLambdaParam (..))
import Elara.AST.Region (Located (Located), SourceRegion (..), enclosingRegion, generatedSourceRegion, sourceRegion, spanningRegion, unlocated, withLocationOf)
import Elara.AST.VarRef (VarRef, VarRef' (Global, Local), withName)
import Elara.Data.AtLeast2List (AtLeast2List (AtLeast2List))
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Data.Unique.Effect
import Elara.Desugar.Error (DesugarError)
import Elara.Error (runErrorAsElaraError)
import Elara.Logging (StructuredDebug, logDebug)
import Elara.Prim (KnownType (..), KnownTypeInfo (..), WiredInPrim (..), knownTypeInfo)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Query
import Elara.Query.Effects
import Elara.Query.Errors ()
import Elara.Rename.Error
import Elara.Rename.Imports (expositionToLocatedName, isExposition, isImportedBy)
import Elara.Rename.State
import Elara.Rules.Generic
import Print (showColored)

import Elara.AST.Module qualified as NewModule
import Elara.AST.Phases.Desugared qualified as NewD
import Elara.AST.Phases.Renamed qualified as NewR
import Elara.AST.Types qualified as New
import Elara.Data.AtLeast2List qualified as AtLeast2List
import Elara.Prim qualified as Prim
import Rock qualified

type Rename r =
    ( Eff.State RenameState :> r
    , Eff.Error RenameError :> r
    , UniqueGen :> r
    , QueryEffects r
    , StructuredDebug :> r
    , Rock.Rock Elara.Query.Query :> r
    , HasCallStack
    )

type InnerRename r =
    ( Eff.State RenameState :> r
    , Eff.Error RenameError :> r
    , UniqueGen :> r
    , QueryEffects r
    , StructuredDebug :> r
    , Eff.Reader (Maybe (NewModule.Module SourceRegion NewD.Desugared)) :> r -- the module we're renaming
    , HasCallStack
    )

instance RunPhase Renamed where
    getModuleByName mn = do
        m <- runErrorAsElaraError @DesugarError $ Rock.fetch $ Elara.Query.DesugaredModule mn
        let NewModule.Module _ m' = m
        let actualName = m'.moduleName ^. unlocated
        when (actualName /= mn) $ throwError $ ModuleNameMismatch (Located (GeneratedRegion "Renaming Entry Point") mn) (actualName `withLocationOf` stripTag m'.moduleName)
        Local.evalState primitiveRenameState $ rename m

    getDeclarationByName = genericGetDeclarationByName @Renamed getModuleByName
    getRequiredDeclarationByName = genericGetRequiredDeclarationByName @Renamed getDeclarationByName
    getConstructorDeclaration = genericGetConstructorDeclaration @Renamed getModuleByName
    getDeclarationAnnotations = genericGetDeclarationAnnotations @Renamed getRequiredDeclarationByName
    getDeclarationAnnotationsOfType = genericGetDeclarationAnnotationsOfType @Renamed getDeclarationAnnotations getConstructorDeclaration

qualifyIn :: Rename r => ModuleName -> MaybeQualified name -> Eff r (Qualified name)
qualifyIn mn (MaybeQualified n (Just m)) = do
    when (m /= mn) $ throwError $ QualifiedInWrongModule m mn
    pure $ Qualified n m
qualifyIn mn (MaybeQualified n Nothing) = pure $ Qualified n mn

qualifyTypeName :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => TaggedLocate TypeNode SourceRegion (MaybeQualified TypeName) -> Eff r (TaggedLocate TypeNode SourceRegion (Qualified TypeName))
qualifyTypeName locName = do
    let sr = unwrapLoc (getLocation locName)
    case locName ^. unlocated of
        MaybeQualified n (Just m) -> do
            let moduleName = TaggedLocate (wrap @ModuleNode sr) m
            ensureExistsAndExposed moduleName (Located sr (NameType n))
            pure $ Qualified n m <$ locName
        MaybeQualified n Nothing -> do
            typeNames' <- use' (field' @"typeNames")
            m <- Eff.ask
            case Map.lookup n typeNames' of
                Nothing -> throwError $ UnknownName (Located sr (NameType n)) m typeNames'
                Just ((Global v) :| []) -> pure $ v ^. unlocated <$ locName
                Just ((Local _) :| []) -> error "can't have local type names"
                Just many -> throwError $ AmbiguousTypeName (Located sr (NameType n)) many

askCurrentModule :: InnerRename r => Eff r (NewModule.Module SourceRegion NewD.Desugared)
askCurrentModule = do
    m <- Eff.ask
    case m of
        Nothing -> throwError UnknownCurrentModule
        Just m' -> pure m'

lookupGenericName ::
    (UniqueGen :> r, Rock.Rock Elara.Query.Query :> r, _) =>
    (Ord name, ToName name, Show name) =>
    Lens' RenameState (Map name (NonEmpty (VarRef name))) ->
    (Located Name -> NonEmpty (VarRef name) -> RenameError) ->
    TaggedLocate node SourceRegion (MaybeQualified name) ->
    Eff r (TaggedLocate node SourceRegion (VarRef name))
lookupGenericName _ _ (TaggedLocate loc (MaybeQualified n (Just m))) = do
    let sr = unwrapLoc loc
    let moduleName = TaggedLocate (wrap @ModuleNode sr) m
    ensureExistsAndExposed moduleName (Located sr (toName n))
    pure $ TaggedLocate loc $ Global (Located sr (Qualified n m))
lookupGenericName lens ambiguousError (TaggedLocate loc (MaybeQualified n Nothing)) = do
    let sr = unwrapLoc loc
    names' <- use' lens
    m <- Eff.ask
    case m of
        Nothing ->
            case Map.lookup n names' of
                Nothing -> throwError $ UnknownName (Located sr $ toName n) m names'
                Just (v :| []) -> pure $ TaggedLocate loc v
                Just many -> throwError $ ambiguousError (Located sr $ toName n) many
        Just m' -> case maybe [] (NonEmpty.filter ((m' `isImportedBy`) . fmap toName)) (Map.lookup n names') of
            [v] -> pure $ TaggedLocate loc v
            [] -> throwError $ UnknownName (Located sr $ toName n) (Just m') names'
            (x : xs) -> throwError $ ambiguousError (Located sr $ toName n) (x :| xs)

lookupVarName :: _ => TaggedLocate VarNode SourceRegion (MaybeQualified VarName) -> Eff r (TaggedLocate VarNode SourceRegion (VarRef VarName))
lookupVarName = lookupGenericName (field' @"varNames") AmbiguousVarName

lookupTypeName :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => TaggedLocate TypeNode SourceRegion (MaybeQualified TypeName) -> Eff r (TaggedLocate TypeNode SourceRegion (Qualified TypeName))
lookupTypeName n = do
    TaggedLocate loc ref <- lookupGenericName (field' @"typeNames") AmbiguousTypeName n
    case ref of
        Local _ -> error "can't have local type names"
        Global v -> pure $ TaggedLocate loc (v ^. unlocated)

lookupTypeVar :: _ => LowerAlphaName -> Eff r (Maybe (Unique LowerAlphaName))
lookupTypeVar n = do
    typeVars' <- use' (field' @"typeVars")
    pure $ Map.lookup n typeVars'

uniquify :: UniqueGen :> r => TaggedLocate n loc name -> Eff r (TaggedLocate n loc (Unique name))
uniquify (TaggedLocate loc name) = TaggedLocate loc <$> makeUnique name

-- | Performs a topological sort of declarations
sortDeclarations :: [NewR.RenamedDeclaration] -> Eff r [NewR.RenamedDeclaration]
sortDeclarations = pure

{- | Rename a module. This involves a few steps:
1. Add all imports to context
2. Add own declarations to context
3. Rename exposing, imports, and declarations
-}
rename :: Rename r => NewModule.Module SourceRegion NewD.Desugared -> Eff r (NewModule.Module SourceRegion NewR.Renamed)
rename m@(NewModule.Module loc m') = do
    addImportsToContext m'.moduleImports
    traverse_ addDeclarationToContext m'.moduleDeclarations
    exposing' <- renameExposing (m'.moduleName ^. unlocated) m'.moduleExposing
    imports' <- traverse renameImport m'.moduleImports
    declarations' <- Eff.runReader (Just m) (traverse renameDeclaration m'.moduleDeclarations)
    sorted <- sortDeclarations declarations'
    pure (NewModule.Module loc (NewModule.Module' m'.moduleName exposing' imports' sorted))
  where
    renameExposing ::
        Rename r =>
        ModuleName ->
        NewModule.Exposing SourceRegion NewD.Desugared ->
        Eff r (NewModule.Exposing SourceRegion NewR.Renamed)
    renameExposing _ NewModule.ExposingAll = pure NewModule.ExposingAll
    renameExposing mn (NewModule.ExposingSome es) = NewModule.ExposingSome <$> traverse (renameExposition mn) es

    renameExposingOrHiding ::
        Rename r =>
        ModuleName ->
        NewModule.ImportExposingOrHiding SourceRegion NewD.Desugared ->
        Eff r (NewModule.ImportExposingOrHiding SourceRegion NewR.Renamed)
    renameExposingOrHiding mn (NewModule.ImportExposing exp) = NewModule.ImportExposing <$> renameExposing mn exp
    renameExposingOrHiding mn (NewModule.ImportHiding hid) = NewModule.ImportHiding <$> traverse (renameExposition mn) hid

    renameExposition :: Rename r => ModuleName -> NewModule.Exposition SourceRegion NewD.Desugared -> Eff r (NewModule.Exposition SourceRegion NewR.Renamed)
    renameExposition mn (NewModule.ExposedValue vn) = do
        let loc = getLocation vn
        let sr = unwrapLoc loc
        qn <- qualifyIn mn (vn ^. unlocated)
        let varRef = Global (Located sr qn)
        pure $ NewModule.ExposedValue (TaggedLocate loc varRef)
    renameExposition mn (NewModule.ExposedOp opn) = do
        let loc = getLocation opn
        let sr = unwrapLoc loc
        qn <- qualifyIn mn (opn ^. unlocated)
        let varRef = Global (Located sr qn)
        pure $ NewModule.ExposedOp (TaggedLocate loc varRef)
    renameExposition mn (NewModule.ExposedType tn) = do
        let loc = getLocation tn
        qn <- qualifyIn mn (tn ^. unlocated)
        pure $ NewModule.ExposedType (TaggedLocate loc qn)
    renameExposition mn (NewModule.ExposedTypeAndAllConstructors tn) = do
        let loc = getLocation tn
        qn <- qualifyIn mn (tn ^. unlocated)
        pure $ NewModule.ExposedTypeAndAllConstructors (TaggedLocate loc qn)

    renameImport :: Rename r => NewModule.Import SourceRegion NewD.Desugared -> Eff r (NewModule.Import SourceRegion NewR.Renamed)
    renameImport (NewModule.Import iloc (NewModule.Import' name as' qual exp')) = do
        exp'' <- renameExposingOrHiding (name ^. unlocated) exp'
        pure $ NewModule.Import iloc (NewModule.Import' name as' qual exp'')

addImportsToContext :: Rename r => [NewModule.Import SourceRegion NewD.Desugared] -> Eff r ()
addImportsToContext = traverse_ addImportToContext

addImportToContext :: Rename r => NewModule.Import SourceRegion NewD.Desugared -> Eff r ()
addImportToContext (NewModule.Import _ imp) =
    addModuleToContext
        imp.importModuleName
        imp.importExposingOrHiding
        imp.importQualified

getModuleFromName ::
    Rename r =>
    TaggedLocate
        ModuleNode
        SourceRegion
        ModuleName ->
    Eff r (NewModule.Module SourceRegion NewD.Desugared)
getModuleFromName mn = do
    m <-
        runErrorAsElaraError @DesugarError $
            Rock.fetch (Elara.Query.DesugaredModule (mn ^. unlocated))
    let NewModule.Module _ m' = m
    let actualName = m'.moduleName ^. unlocated
    when (actualName /= mn ^. unlocated) $
        throwError $
            ModuleNameMismatch
                (stripTag mn)
                (actualName `withLocationOf` stripTag m'.moduleName)
    pure m

{- | Add all exposed declarations from a module to the renaming context.
This is used when we import a module, adding all the imported names to the context so we can resolve them when we see them in the code.
-}
addModuleToContext ::
    Rename r =>
    TaggedLocate ModuleNode SourceRegion ModuleName ->
    NewModule.ImportExposingOrHiding SourceRegion NewD.Desugared ->
    Bool ->
    Eff r ()
addModuleToContext mn importSpec qualified = do
    imported <- getModuleFromName mn
    let NewModule.Module _ importedMod = imported
    let allDecls = importedMod.moduleDeclarations
    let thisMn = mn ^. unlocated

    let declName' = toName . declarationName
    let moduleExported = filter (isExposingAndExists imported . declName') allDecls

    toAdd <- case importSpec of
        NewModule.ImportExposing NewModule.ExposingAll ->
            pure moduleExported
        NewModule.ImportExposing (NewModule.ExposingSome importList) -> do
            for_ importList $ \expo ->
                unless (any (\d -> isExposition thisMn (declName' d) expo) moduleExported) $
                    throwError $
                        NonExistentModuleDeclaration thisMn (expositionToLocatedName expo)
            pure $ filter (\d -> any (isExposition thisMn (declName' d)) importList) moduleExported
        NewModule.ImportHiding hidingList -> do
            for_ hidingList $ \expo ->
                unless (any (\d -> isExposition thisMn (declName' d) expo) moduleExported) $
                    throwError $
                        NonExistentModuleDeclaration thisMn (expositionToLocatedName expo)
            pure $ filter (\d -> not $ any (isExposition thisMn (declName' d)) hidingList) moduleExported

    unless qualified $
        traverse_ addDeclarationToContext toAdd

    when qualified $ do
        let (ModuleName parts) = thisMn
        let simpleModuleName = last parts
        let isPrincipalType decl =
                case declarationName decl of
                    DeclType tn -> tn == TypeName simpleModuleName
                    _ -> False
        traverse_ addDeclarationToContext (filter isPrincipalType toAdd)

-- | Get the name of a declaration, preserving the VarName/TypeName distinction.
declarationName :: New.Declaration SourceRegion NewD.Desugared -> DeclName
declarationName (New.Declaration _ (New.Declaration' _ body)) =
    let New.DeclarationBody _ body' = body
     in case body' of
            New.ValueDeclaration n _ _ _ _ _ -> DeclVar (n ^. unlocated)
            New.TypeDeclarationBody n _ _ _ _ _ -> DeclType (n ^. unlocated)
            New.DeclBodyExtension v -> absurd v

-- | Add a declaration to the renaming state.
addDeclarationToContext ::
    Rename r =>
    New.Declaration SourceRegion NewD.Desugared ->
    Eff r ()
addDeclarationToContext decl@(New.Declaration _dloc (New.Declaration' declMN body)) = do
    let New.DeclarationBody _ body' = body
    let nameLoc :: SourceRegion = case body' of
            New.ValueDeclaration n _ _ _ _ _ -> unwrapLoc (getLocation n)
            New.TypeDeclarationBody n _ _ _ _ _ -> unwrapLoc (getLocation n)
            New.DeclBodyExtension v -> absurd v

    -- create a global var ref for a given name
    -- uses the declaration's module name for qualification
    -- and the var ref's location refers to just the declaration name
    let global :: name -> VarRef name
        global vn =
            let mn = declMN ^. unlocated
             in Global (Qualified vn mn `withLocationOf` nameLoc)

    case declarationName decl of
        DeclVar vn -> Eff.modify $ over (the @"varNames") $ insertMerging vn (global vn)
        DeclType tn -> Eff.modify $ over (the @"typeNames") $ insertMerging tn (global tn)

    logDebug $ "Added declaration to context: " <> pretty (declarationName decl)

    case body' of
        -- Add all the constructor names to context
        New.TypeDeclarationBody _ _ (New.ADT ctors) _ _ _ -> for_ ctors $ \(cn, _) ->
            let tn = cn ^. unlocated
             in Eff.modify $ over (the @"typeNames") $ insertMerging tn (global tn)
        _ -> pass

-- | Ensure that a name exists in the context and is exposed
ensureExistsAndExposed :: (Rock.Rock Elara.Query.Query :> r, _) => TaggedLocate ModuleNode SourceRegion ModuleName -> Located Name -> Eff r ()
ensureExistsAndExposed mn n = do
    thisMod <- Eff.ask
    m <- getModuleFromName mn
    unless (elementExistsInModule m (n ^. unlocated)) $ throwError $ NonExistentModuleDeclaration (mn ^. unlocated) n
    unless (isExposingAndExists m (n ^. unlocated)) $ throwError $ UnknownName @Name n thisMod mempty

elementExistsInModule :: NewModule.Module SourceRegion NewD.Desugared -> Name -> Bool
elementExistsInModule (NewModule.Module _ m') n' =
    any
        ( \decl ->
            toName (declarationName decl) == n'
                || case decl of
                    New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ (New.TypeDeclarationBody _ _ (New.ADT ctors) _ _ _))) ->
                        any (\(cn, _) -> NameType (cn ^. unlocated) == n') ctors
                    _ -> False
        )
        m'.moduleDeclarations

{- | Tests that n is exposed in m
I.e. that it is in the exposing list, or that the module is exposing everything
-}
isExposingAndExists :: NewModule.Module SourceRegion NewD.Desugared -> Name -> Bool
isExposingAndExists m@(NewModule.Module _ m') n =
    let mn = m'.moduleName ^. unlocated
     in case m'.moduleExposing of
            NewModule.ExposingAll -> elementExistsInModule m n
            NewModule.ExposingSome es -> elementExistsInModule m n && any (isExposition mn n) es
  where
    isExposition :: ModuleName -> Name -> NewModule.Exposition SourceRegion NewD.Desugared -> Bool
    isExposition mn (NameValue vn) (NewModule.ExposedValue vn') = MaybeQualified (NormalVarName vn) (Just mn) == vn' ^. unlocated
    isExposition mn (NameOp vn) (NewModule.ExposedValue vn') = MaybeQualified (OperatorVarName vn) (Just mn) == vn' ^. unlocated
    isExposition mn (NameOp vn) (NewModule.ExposedOp opn') = MaybeQualified vn (Just mn) == opn' ^. unlocated
    isExposition mn (NameType tn) (NewModule.ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition mn (NameType tn) (NewModule.ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition _ _ _ = False

renameDeclaration :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => New.Declaration SourceRegion NewD.Desugared -> Eff r NewR.RenamedDeclaration
renameDeclaration decl@(New.Declaration dloc (New.Declaration' mn body)) = do
    body' <- Eff.runReader (Just decl) $ renameDeclarationBody body
    pure $ New.Declaration dloc (New.Declaration' mn body')
  where
    renameDeclarationBody :: (InnerRename r, Rock.Rock Elara.Query.Query :> r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r) => New.DeclarationBody SourceRegion NewD.Desugared -> Eff r NewR.RenamedDeclarationBody
    renameDeclarationBody (New.DeclarationBody bloc body') = New.DeclarationBody bloc <$> renameDeclarationBody' body'

    renameDeclarationBody' :: (InnerRename r, Rock.Rock Elara.Query.Query :> r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r) => New.DeclarationBody' SourceRegion NewD.Desugared -> Eff r NewR.RenamedDeclarationBody'
    renameDeclarationBody' (New.ValueDeclaration name val _pats _mTy mTypeMeta anns) = scoped $ do
        mTypeMeta' <- traverse renameSimpleTypeAllowNewVars mTypeMeta
        val' <- renameExpr val
        anns' <- traverse renameAnnotation anns
        thisModule <- askCurrentModule
        let NewModule.Module _ thisMod = thisModule
        let qualifiedName =
                (\n -> Qualified n (thisMod.moduleName ^. unlocated)) <$> name
        pure $ New.ValueDeclaration qualifiedName val' () () mTypeMeta' anns'
    renameDeclarationBody' (New.TypeDeclarationBody name vars typeDecl _mKind _meta anns) = do
        vars' <- traverse uniquify vars
        let varAliases = zip vars vars'
        let addAllVarAliases s =
                foldl'
                    (\s' (vn, uniqueVn) -> the @"typeVars" %~ Map.insert (vn ^. unlocated) (uniqueVn ^. unlocated) $ s')
                    s
                    varAliases
        let declModuleName = mn ^. unlocated
        locally addAllVarAliases $ do
            thisModule <- askCurrentModule
            let NewModule.Module _ thisMod = thisModule
            let qualifiedName =
                    (\n -> Qualified n (thisMod.moduleName ^. unlocated)) <$> name
            typeDecl' <- renameTypeDeclaration declModuleName qualifiedName typeDecl
            anns' <- traverse renameAnnotation anns
            pure $ New.TypeDeclarationBody qualifiedName vars' typeDecl' Nothing NoExtension anns'
    renameDeclarationBody' (New.DeclBodyExtension v) = absurd v

renameAnnotation :: (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) => New.Annotation SourceRegion NewD.Desugared -> Eff r (New.Annotation SourceRegion NewR.Renamed)
renameAnnotation (New.Annotation name args) = do
    name' <- qualifyTypeName name
    args' <- traverse (\(New.AnnotationArg e) -> New.AnnotationArg <$> renameExpr e) args
    pure $ New.Annotation name' args'

renameTypeDeclaration :: _ => ModuleName -> TaggedLocate TypeNode SourceRegion (Qualified TypeName) -> New.TypeDeclaration SourceRegion NewD.Desugared -> Eff r (New.TypeDeclaration SourceRegion NewR.Renamed)
renameTypeDeclaration _ declarationName' (New.Alias aliasedType) = do
    t' <- renameSimpleType aliasedType
    let isRecursive = typeIsRecursive (declarationName' ^. unlocated) t'
    whenJust isRecursive $ \r -> do
        logDebug
            ( "Detected recursive type alias: "
                <> pretty (showColored r :: Text)
                <> " at "
                <> pretty (r ^. sourceRegion)
            )
        throwError $ RecursiveTypeAlias (stripTag declarationName') r

    pure $ New.Alias t'
renameTypeDeclaration thisMod _declarationName' (New.ADT constructors) = do
    constructors' <-
        traverse
            (\(n, tys) -> (over unlocated (`Qualified` thisMod) n,) <$> traverse renameSimpleType tys)
            constructors
    pure $ New.ADT constructors'

renameSimpleType :: _ => New.Type SourceRegion NewD.Desugared -> Eff r NewR.RenamedType
renameSimpleType = renameSimpleTypeWith False

renameSimpleTypeAllowNewVars :: _ => New.Type SourceRegion NewD.Desugared -> Eff r NewR.RenamedType
renameSimpleTypeAllowNewVars = renameSimpleTypeWith True

renameSimpleTypeWith :: _ => Bool -> New.Type SourceRegion NewD.Desugared -> Eff r NewR.RenamedType
renameSimpleTypeWith antv (New.Type loc () t') = do
    t'' <- renameType antv t'
    pure $ New.Type loc () t''

-- | Renames a type, qualifying type constructors and type variables where necessary
renameType ::
    (InnerRename r, Rock.Rock Elara.Query.Query :> r) =>
    {- | If new type variables are allowed - if 'False', this will throw an error if a type variable is not in scope.
    This is useful for type declarations, where something like @type Invalid a = b@ would clearly be invalid
    But for local type annotations, we want to allow this, as it may be valid to have new type variables there - eg @\x -> (x : a)@
    -}
    Bool ->
    New.Type' SourceRegion NewD.Desugared ->
    Eff r NewR.RenamedType'
renameType allowNewTypeVars (New.TVar (TaggedLocate l n)) = do
    inCtx <- lookupTypeVar n
    case inCtx of
        Just inCtx' -> pure $ New.TVar (TaggedLocate l inCtx')
        Nothing
            | allowNewTypeVars -> do
                uniqueN <- makeUnique n
                Eff.modify $ over (the @"typeVars") $ Map.insert n uniqueN
                pure (New.TVar $ TaggedLocate l uniqueN)
            | otherwise -> throwError $ UnknownTypeVariable n
renameType antv (New.TFun t1 t2) = New.TFun <$> renameSimpleTypeWith antv t1 <*> renameSimpleTypeWith antv t2
renameType _ New.TUnit = do
    -- turn it into Elara.Prim.()
    let unitTypeName = knownQualified (knownTypeInfo (KnownWiredIn WiredInUnit))
    let unitLoc = wrap @TypeNode (generatedSourceRegion Nothing)
    pure $ New.TUserDefined (TaggedLocate unitLoc unitTypeName)
renameType antv (New.TApp t1 t2) = New.TApp <$> renameSimpleTypeWith antv t1 <*> renameSimpleTypeWith antv t2
renameType _ (New.TUserDefined ln) = New.TUserDefined <$> qualifyTypeName ln
renameType antv (New.TRecord fields) = New.TRecord <$> traverse (traverseOf _2 (renameSimpleTypeWith antv)) fields
renameType antv (New.TList t) = New.TList <$> renameSimpleTypeWith antv t
renameType antv (New.TExtension (TupleType (AtLeast2List fst' snd' []))) = do
    -- turn it into Elara.Prim.Tuple2 type
    fst'' <- renameSimpleTypeWith antv fst'
    snd'' <- renameSimpleTypeWith antv snd'
    let New.Type fstLoc _ _ = fst'
    let New.Type sndLoc _ _ = snd'
    let loc = enclosingRegion fstLoc sndLoc
    let tupleCtor = New.Type loc () (New.TUserDefined (TaggedLocate loc Prim.tuple2CtorName))
    let base = New.TApp tupleCtor fst''

    pure $ New.TApp (New.Type loc () base) snd''
renameType antv (New.TExtension (TupleType tupleElems)) = do
    -- turn it into Elara.Prim.TupleN type
    let tupleName = Prim.tupleNCtorName (length tupleElems)

    elems' <- traverse (renameSimpleTypeWith antv) tupleElems
    let loc = foldr1 enclosingRegion (fmap (\(New.Type l _ _) -> l) elems')
    let tupleCtor = New.Type loc () (New.TUserDefined (TaggedLocate loc tupleName))

    let (head', tail) = AtLeast2List.toHeadAndTail elems'
    pure $ foldl' (New.TApp . New.Type loc ()) (New.TApp tupleCtor head') tail

renameExpr :: (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) => NewD.DesugaredExpr -> Eff r NewR.RenamedExpr
renameExpr (New.Expr _ () (New.EBlock es)) = desugarBlock es
renameExpr e@(New.Expr _ () (New.ELet{})) = desugarBlock (e :| [])
renameExpr (New.Expr loc () e') = do
    (e'', meta) <- renameExpr' loc e'
    pure $ New.Expr loc meta e''
  where
    renameExpr' ::
        (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) =>
        NodeLoc ExprNode SourceRegion -> New.Expr' SourceRegion NewD.Desugared -> Eff r (NewR.RenamedExpr', Maybe NewR.RenamedType)
    renameExpr' _ (New.EInt i) = pure (New.EInt i, Nothing)
    renameExpr' _ (New.EFloat i) = pure (New.EFloat i, Nothing)
    renameExpr' _ (New.EString i) = pure (New.EString i, Nothing)
    renameExpr' _ (New.EChar i) = pure (New.EChar i, Nothing)
    renameExpr' _ New.EUnit = pure (New.EUnit, Nothing)
    renameExpr' _ (New.EVar NoExtension i) = do
        i' <- lookupVarName i
        pure (New.EVar NoExtension i', Nothing)
    renameExpr' _ (New.ECon NoExtension i) = do
        i' <- lookupTypeName i
        pure (New.ECon NoExtension i', Nothing)
    renameExpr' _ (New.ELam NoExtension pat body) = do
        (e'', meta) <- renameLambda pat body
        pure (e'', meta)
    renameExpr' _ (New.EApp NoExtension e1 e2) = do
        e1' <- renameExpr e1
        e2' <- renameExpr e2
        pure (New.EApp NoExtension e1' e2', Nothing)
    renameExpr' _ (New.ETyApp e1 t1) = do
        e1' <- renameExpr e1
        t1' <- renameSimpleType t1
        pure (New.ETyApp e1' t1', Nothing)
    renameExpr' _ (New.EIf e1 e2 e3) = do
        e1' <- renameExpr e1
        e2' <- renameExpr e2
        e3' <- renameExpr e3
        pure (New.EIf e1' e2' e3', Nothing)
    renameExpr' _ (New.EMatch e cases) = do
        e' <- renameExpr e
        cases' <- traverse (bitraverse renamePattern renameExpr) cases
        pure (New.EMatch e' cases', Nothing)
    renameExpr' _ (New.ELetIn NoExtension vn e body) = do
        vn' <- uniquify vn
        locally (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ (Local :: Located (Unique VarName) -> VarRef VarName) (stripTag vn'))) $ do
            exp' <- renameExpr e
            body' <- renameExpr body
            pure (New.ELetIn NoExtension vn' exp' body', Nothing)
    renameExpr' _ (New.EAnn e ty) = do
        e' <- renameExpr e
        ty' <- renameSimpleType ty
        let New.Expr _ _ e'' = e'
        pure (e'', Just ty')
    renameExpr' _ (New.EExtension ext) = renameExprExtension ext
    renameExpr' _ (New.EBlock{}) = error "renameExpr': Block should be handled by renameExpr"
    renameExpr' _ (New.ELet{}) = error "renameExpr': Let should be handled by renameExpr"

    renameExprExtension :: (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) => DesugaredExpressionExtension SourceRegion -> Eff r (NewR.RenamedExpr', Maybe NewR.RenamedType)
    renameExprExtension (DesugaredBinaryOperator (BinaryOperatorExpression op left right)) = do
        op' <- renameBinaryOperator op
        left' <- renameExpr left
        right' <- renameExpr right
        pure (New.EExtension (RenamedBinaryOperator (BinaryOperatorExpression op' left' right')), Nothing)
    renameExprExtension (DesugaredInParens (InParensExpression e)) = do
        e' <- renameExpr e
        pure (New.EExtension (RenamedInParens (InParensExpression e')), Nothing)
    renameExprExtension (DesugaredList (ListExpression [])) = do
        let typeLoc = wrap @TypeNode (unwrapLoc loc)
        pure (New.ECon NoExtension (TaggedLocate typeLoc Prim.nilCtorName), Nothing)
    renameExprExtension (DesugaredList (ListExpression (x : xs))) = do
        xs' <- traverse renameExpr (x :| xs)
        let lastCons :: NewR.RenamedExpr =
                let typeLoc = wrap @TypeNode (unwrapLoc loc)
                 in New.Expr (exprLoc (last xs')) Nothing (New.ECon NoExtension (TaggedLocate typeLoc Prim.nilCtorName))
        let cons :: NewR.RenamedExpr -> NewR.RenamedExpr -> NewR.RenamedExpr
            cons x' y =
                let xLoc = exprLoc x'
                    yLoc = exprLoc y
                    typeLoc = wrap @TypeNode (unwrapLoc loc)
                    consE = New.Expr loc Nothing (New.ECon NoExtension (TaggedLocate typeLoc Prim.consCtorName))
                    appConsX = New.Expr xLoc Nothing (New.EApp NoExtension consE x')
                 in New.Expr yLoc Nothing (New.EApp NoExtension appConsX y)
        let createConses :: [NewR.RenamedExpr] -> NewR.RenamedExpr
            createConses [] = lastCons
            createConses (x' : xs'') = cons x' (createConses xs'')
        let result = createConses (toList xs')
        let New.Expr _ meta e'' = result
        pure (e'', meta)
    renameExprExtension (DesugaredTuple (TupleExpression (AtLeast2List fst' snd' []))) = do
        fst'' <- renameExpr fst'
        snd'' <- renameExpr snd'
        let typeLoc = wrap @TypeNode (unwrapLoc loc)
        let base =
                New.Expr
                    loc
                    Nothing
                    ( New.EApp
                        NoExtension
                        ( New.Expr
                            loc
                            Nothing
                            (New.ECon NoExtension (TaggedLocate typeLoc Prim.tuple2CtorName))
                        )
                        fst''
                    )
        pure (New.EApp NoExtension base snd'', Nothing)
    renameExprExtension (DesugaredTuple (TupleExpression tupleElems)) = do
        -- turn it into Elara.Prim.TupleN type
        let tupleName :: Qualified TypeName = Prim.tupleNCtorName (length tupleElems)

        elems' <- traverse renameExpr tupleElems
        let typeLoc = wrap @TypeNode (unwrapLoc loc)
        let headCtorExpr :: New.Expr SourceRegion Renamed =
                New.Expr loc Nothing (New.ECon NoExtension (TaggedLocate typeLoc tupleName))

        let elemsList = AtLeast2List.toNonEmpty elems'
        let New.Expr _ _ r = foldl' (\acc m -> New.Expr loc Nothing (New.EApp NoExtension acc m)) headCtorExpr elemsList

        pure (r, Nothing)

renameBinaryOperator :: forall r. (InnerRename r, Rock.Rock Elara.Query.Query :> r) => New.BinaryOperator SourceRegion NewD.Desugared -> Eff r (New.BinaryOperator SourceRegion NewR.Renamed)
renameBinaryOperator (New.SymOp opLoc occ) = do
    TaggedLocate loc op' <- lookupVarName (fmap OperatorVarName <$> occ)
    let onlyOpName (OperatorVarName o') = o'
        onlyOpName _ = error "renameBinaryOperator: I really don't like this"
    let op'' = onlyOpName <$> op'
    pure $ New.SymOp opLoc (TaggedLocate loc op'')
renameBinaryOperator (New.InfixedOp opLoc (TaggedLocate l o)) = do
    op' <- case o of
        MaybeQualified (NameValue n) q -> do
            vn <- lookupVarName (TaggedLocate l (MaybeQualified (NormalVarName n) q))
            pure $ withName (vn ^. unlocated)
        MaybeQualified (NameOp n) q -> do
            vn <- lookupVarName (TaggedLocate l (MaybeQualified (OperatorVarName n) q))
            pure $ withName (vn ^. unlocated)
        MaybeQualified (NameType n) q -> do
            let typeLoc = wrap @TypeNode (unwrapLoc l)
            tn <- lookupTypeName (TaggedLocate typeLoc (MaybeQualified n q))
            let sr = unwrapLoc l
            let qualifiedName = NameType <$> (tn ^. unlocated)
            pure $ Global (Located sr qualifiedName)
    pure $ New.InfixedOp opLoc op'

renamePattern :: forall r. (InnerRename r, Rock.Rock Elara.Query.Query :> r) => NewD.DesugaredPattern -> Eff r NewR.RenamedPattern
renamePattern (New.Pattern loc meta p') = do
    meta' <- traverse renameSimpleType meta
    p'' <- renamePattern' loc p'
    pure $ New.Pattern loc meta' p''
  where
    renamePattern' :: NodeLoc PatternNode SourceRegion -> NewD.DesugaredPattern' -> Eff r NewR.RenamedPattern'
    renamePattern' _ (New.PInt i) = pure $ New.PInt i
    renamePattern' _ (New.PFloat i) = pure $ New.PFloat i
    renamePattern' _ (New.PString i) = pure $ New.PString i
    renamePattern' _ (New.PChar i) = pure $ New.PChar i
    renamePattern' _ New.PWildcard = pure New.PWildcard
    renamePattern' _ New.PUnit = pure New.PUnit
    renamePattern' _ (New.PVar vn) = do
        vn' <- uniquify vn
        Eff.modify (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ Local (stripTag vn')))
        pure $ New.PVar vn'
    renamePattern' _ (New.PCon cn ps) = do
        cn' <- qualifyTypeName cn
        ps' <- traverse renamePattern ps
        pure $ New.PCon cn' ps'
    renamePattern' ploc (New.PExtension ext) = renamePatternExtension ploc ext

    renamePatternExtension :: NodeLoc PatternNode SourceRegion -> ListTuplePatternExtension SourceRegion NewD.Desugared -> Eff r NewR.RenamedPattern'
    renamePatternExtension ploc (ListPattern []) = do
        let typeLoc = wrap @TypeNode (unwrapLoc ploc)
        pure $ New.PCon (TaggedLocate typeLoc Prim.nilCtorName) []
    renamePatternExtension ploc (ListPattern (x : xs)) = do
        xs' <- traverse renamePattern (x :| xs)
        let lastCons :: NewR.RenamedPattern =
                let typeLoc = wrap @TypeNode (unwrapLoc ploc)
                 in New.Pattern (exprLocP (last xs')) Nothing (New.PCon (TaggedLocate typeLoc Prim.nilCtorName) [])
        let cons :: NewR.RenamedPattern -> NewR.RenamedPattern -> NewR.RenamedPattern
            cons x' y =
                let typeLoc = wrap @TypeNode (unwrapLoc ploc)
                 in New.Pattern (exprLocP x') Nothing (New.PCon (TaggedLocate typeLoc Prim.consCtorName) [x', y])
        let createConses :: [NewR.RenamedPattern] -> NewR.RenamedPattern
            createConses [] = lastCons
            createConses (x' : xs'') = cons x' (createConses xs'')
        let result = createConses (toList xs')
        let New.Pattern _ _ p'' = result
        pure p''
    renamePatternExtension ploc (ConsPattern p1 p2) = do
        p1' <- renamePattern p1
        p2' <- renamePattern p2
        let typeLoc = wrap @TypeNode (unwrapLoc ploc)
        pure $ New.PCon (TaggedLocate typeLoc Prim.consCtorName) [p1', p2']
    renamePatternExtension ploc (TuplePattern (p1 :| [p2])) = do
        p1' <- renamePattern p1
        p2' <- renamePattern p2
        let typeLoc = wrap @TypeNode (unwrapLoc ploc)
        pure $ New.PCon (TaggedLocate typeLoc Prim.tuple2CtorName) [p1', p2']
    renamePatternExtension _ (TuplePattern _) = error "renamePattern': TuplePattern more than length 2"

-- | Get location from an expression
exprLoc :: New.Expr SourceRegion p -> NodeLoc ExprNode SourceRegion
exprLoc (New.Expr loc _ _) = loc

-- | Get location from a pattern
exprLocP :: New.Pattern SourceRegion p -> NodeLoc PatternNode SourceRegion
exprLocP (New.Pattern loc _ _) = loc

{- | Estimates a var name from a pattern
This isn't really necessary as names will be uniquified anyway, but it could make dumped code more readable
-}
patternToVarName :: NewD.DesugaredPattern -> VarName
patternToVarName (New.Pattern _ _ p) =
    let mn = NormalVarName . LowerAlphaName
     in case p of
            New.PWildcard -> mn "wildcard"
            New.PVar vn -> vn ^. unlocated
            New.PInt _ -> mn "int"
            New.PFloat _ -> mn "float"
            New.PString _ -> mn "string"
            New.PChar _ -> mn "char"
            New.PCon _ _ -> mn "constructor"
            New.PUnit -> "unit"
            New.PExtension (ListPattern _) -> mn "list"
            New.PExtension (ConsPattern _ _) -> mn "cons"
            New.PExtension (TuplePattern _) -> mn "tuple"

-- | Turn a pattern and a body into a variable and a match expression. Used for renaming lambdas who use patterns as binders.
patternToMatch ::
    (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) =>
    -- | Pattern to turn into a match
    NewD.DesugaredPattern ->
    -- | Body of the lambda
    NewD.DesugaredExpr ->
    -- | The variable to bind the match to, and the match expression
    Eff r (TaggedLocate VarNode SourceRegion (Unique VarName), NewR.RenamedExpr)
patternToMatch (New.Pattern _ _ (New.PVar vn)) body = do
    -- Special case, no match needed
    -- vn :: Located VarName
    uniqueVn <- uniquify vn
    body' <- locally (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ (Local :: Located (Unique VarName) -> VarRef VarName) (stripTag uniqueVn))) $ renameExpr body
    pure (uniqueVn, body')
patternToMatch pat body = do
    let vn = patternToVarName pat
    let patLocation = exprLocP pat
    let bodyLocation = exprLoc body
    let varLoc = wrap @VarNode (unwrapLoc patLocation)
    uniqueVn <- uniquify (TaggedLocate varLoc vn)
    let varRef = TaggedLocate (getLocation uniqueVn) (Local (stripTag uniqueVn))
    pat' <- renamePattern pat
    body' <- renameExpr body
    let match =
            New.EMatch
                (New.Expr (widen (exprLocP pat')) Nothing (New.EVar NoExtension varRef))
                [(pat', body')]
    pure (uniqueVn, New.Expr (patLocation <.> bodyLocation) Nothing match)

{- | Rename a lambda expression.
This is a little bit special because patterns have to be converted to match expressions.

For example,
@\(a, b) -> a@  becomes @\ab_ -> match ab_ with (a, b) -> a@
-}
renameLambda :: (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) => NewD.DesugaredPattern -> NewD.DesugaredExpr -> Eff r (NewR.RenamedExpr', Maybe NewR.RenamedType)
renameLambda p@(New.Pattern _ argType _) e = do
    (arg, match) <- patternToMatch p e
    argType' <- traverse renameSimpleType argType
    let binder = TypedLambdaParam (arg ^. unlocated) argType'
    pure (New.ELam NoExtension binder match, Nothing)

desugarBlock :: (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) => NonEmpty NewD.DesugaredExpr -> Eff r NewR.RenamedExpr
desugarBlock (e@(New.Expr _ () (New.ELet{})) :| []) = do
    decl <- Eff.ask @(Maybe (New.Declaration SourceRegion NewD.Desugared))
    throwError (BlockEndsWithLet e (fmap (\(New.Declaration _ (New.Declaration' _ body)) -> body) decl))
desugarBlock (e :| []) = renameExpr e
desugarBlock (New.Expr l () (New.ELet NoExtension n val) :| (xs1 : xs')) = do
    n' <- uniquify n
    locally (the @"varNames" %~ Map.insert (n ^. unlocated) (one $ Local (stripTag n'))) $ do
        val' <- renameExpr val
        block <- desugarBlock (xs1 :| xs')
        pure $ New.Expr l Nothing (New.ELetIn NoExtension n' val' block)
desugarBlock xs = do
    let loc = spanningRegion (xs <&> exprLoc)
    xs' <- traverse renameExpr xs
    pure $ New.Expr loc Nothing (New.EBlock xs')

-- | Checks if a type is recursive with respect to a target type, returning the use of the target type if so
typeIsRecursive :: Qualified TypeName -> NewR.RenamedType -> Maybe (Located (Qualified TypeName))
typeIsRecursive targetType (New.Type _loc () t) = case t of
    New.TVar _ -> Nothing
    New.TFun a b -> typeIsRecursive targetType a <|> typeIsRecursive targetType b
    New.TUnit -> Nothing
    New.TApp a b -> typeIsRecursive targetType a <|> typeIsRecursive targetType b
    New.TUserDefined locatedName ->
        if locatedName ^. unlocated == targetType
            then Just (stripTag locatedName)
            else Nothing
    New.TRecord fields -> asum (fmap (typeIsRecursive targetType . snd) fields)
    New.TList t' -> typeIsRecursive targetType t'
    New.TExtension v -> absurd v
