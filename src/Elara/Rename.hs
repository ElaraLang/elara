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

import Data.Foldable1 (foldr1)
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
import Elara.AST.Instances ()
import Elara.AST.Location
import Elara.AST.Module
import Elara.AST.Name (DeclName (..), LowerAlphaName (..), MaybeQualified (MaybeQualified), ModuleName (..), Name (..), Qualified (Qualified), ToName (toName), TypeName (..), VarName (..))
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Desugared
import Elara.AST.Phases.Renamed
import Elara.AST.Region (Located (Located), SourceRegion (..), enclosingRegion, generatedSourceRegion, sourceRegion, unlocated, withLocationOf)
import Elara.AST.Types
import Elara.AST.VarRef (VarRef, VarRef' (Global, Local), withName)
import Elara.Data.AtLeast2List (AtLeast2List (AtLeast2List))
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Data.Unique.Effect
import Elara.Error (runErrorAsElaraError)
import Elara.Logging (StructuredDebug, logDebug)
import Elara.Prim (KnownType (..), KnownTypeInfo (..), WiredInPrim (..), knownTypeInfo)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Query
import Elara.Query.Effects
import Elara.Rename.Error
import Elara.Rename.Imports (expositionToLocatedName, isExposition, isImportedBy)
import Elara.Rename.State
import Elara.Rules.Generic
import Print (showColored)

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
    , Eff.Reader (Maybe (Module SourceRegion Desugared)) :> r -- the module we're renaming
    , HasCallStack
    )

instance RunPhase Renamed where
    getModuleByName mn = runErrorAsElaraError @RenameError $ do
        m <- Rock.fetch $ Elara.Query.DesugaredModule mn
        let Module _ m' = m
        let actualName = m'.moduleName ^. unlocated
        when (actualName /= mn) $ throwError $ ModuleNameMismatch (Located (GeneratedRegion "Renaming Entry Point") mn) (actualName `withLocationOf` stripTag m'.moduleName)
        Local.evalState primitiveRenameState $ rename m

    getDeclarationByName = genericGetDeclarationByName @Renamed (Rock.fetch . Elara.Query.ModuleByName @Renamed)
    getRequiredDeclarationByName = genericGetRequiredDeclarationByName @Renamed (Rock.fetch . Elara.Query.DeclarationByName @Renamed)
    getConstructorDeclaration = genericGetConstructorDeclaration @Renamed (Rock.fetch . Elara.Query.ModuleByName @Renamed)
    getDeclarationAnnotations = genericGetDeclarationAnnotations @Renamed (Rock.fetch . Elara.Query.RequiredDeclarationByName @Renamed)
    getDeclarationAnnotationsOfType = genericGetDeclarationAnnotationsOfType @Renamed (Rock.fetch . Elara.Query.DeclarationAnnotations @Renamed) (Rock.fetch . Elara.Query.ConstructorDeclaration @Renamed) stripTagAndLocation

qualifyIn :: Rename r => ModuleName -> MaybeQualified name -> Eff r (Qualified name)
qualifyIn mn (MaybeQualified n (Just m)) = do
    when (m /= mn) $ throwError $ QualifiedInWrongModule m mn
    pure $ Qualified n m
qualifyIn mn (MaybeQualified n Nothing) = pure $ Qualified n mn

qualifyTypeName :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => TaggedLocate TypeNode SourceRegion (MaybeQualified TypeName) -> Eff r (TaggedLocate TypeNode SourceRegion (Qualified TypeName))
qualifyTypeName locName = do
    let sr = view sourceRegion locName
    case locName ^. unlocated of
        MaybeQualified n (Just m) -> do
            let moduleName = TaggedLocate (wrap ModuleNode sr) m
            ensureExistsAndExposed moduleName (Located sr (NameType n))
            pure $ Qualified n m <$ locName
        MaybeQualified n Nothing -> do
            typeNames' <- use' (field' @"typeNames" @RenameState)
            m <- Eff.ask
            case Map.lookup n typeNames' of
                Nothing -> throwError $ UnknownName (Located sr (NameType n)) m typeNames'
                Just ((Global v) :| []) -> pure $ v ^. unlocated <$ locName
                Just ((Local _) :| []) -> error "can't have local type names"
                Just many -> throwError $ AmbiguousTypeName (Located sr (NameType n)) many

askCurrentModule :: InnerRename r => Eff r (Module SourceRegion Desugared)
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
    let sr = view sourceRegion loc
    let moduleName = TaggedLocate (wrap ModuleNode sr) m
    ensureExistsAndExposed moduleName (Located sr (toName n))
    pure $ TaggedLocate loc $ Global (Located sr (Qualified n m))
lookupGenericName lens ambiguousError (TaggedLocate loc (MaybeQualified n Nothing)) = do
    let sr = view sourceRegion loc
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
    typeVars' <- use' (field' @"typeVars" @RenameState)
    pure $ Map.lookup n typeVars'

uniquify :: UniqueGen :> r => TaggedLocate n loc name -> Eff r (TaggedLocate n loc (Unique name))
uniquify (TaggedLocate loc name) = TaggedLocate loc <$> makeUnique name

-- | Performs a topological sort of declarations
sortDeclarations :: [RenamedDeclaration] -> Eff r [RenamedDeclaration]
sortDeclarations = pure

{- | Rename a module. This involves a few steps:
1. Add all imports to context
2. Add own declarations to context
3. Rename exposing, imports, and declarations
-}
rename :: Rename r => Module SourceRegion Desugared -> Eff r (Module SourceRegion Renamed)
rename m@(Module loc m') = do
    addImportsToContext m'.moduleImports
    traverse_ addDeclarationToContext m'.moduleDeclarations
    exposing' <- renameExposing (m'.moduleName ^. unlocated) m'.moduleExposing
    imports' <- traverse renameImport m'.moduleImports
    declarations' <- Eff.runReader (Just m) (traverse renameDeclaration m'.moduleDeclarations)
    sorted <- sortDeclarations declarations'
    pure (Module loc (Module' m'.moduleName exposing' imports' sorted))
  where
    renameExposing ::
        Rename r =>
        ModuleName ->
        Exposing SourceRegion Desugared ->
        Eff r (Exposing SourceRegion Renamed)
    renameExposing _ ExposingAll = pure ExposingAll
    renameExposing mn (ExposingSome es) = ExposingSome <$> traverse (renameExposition mn) es

    renameExposingOrHiding ::
        Rename r =>
        ModuleName ->
        ImportExposingOrHiding SourceRegion Desugared ->
        Eff r (ImportExposingOrHiding SourceRegion Renamed)
    renameExposingOrHiding mn (ImportExposing exp) = ImportExposing <$> renameExposing mn exp
    renameExposingOrHiding mn (ImportHiding hid) = ImportHiding <$> traverse (renameExposition mn) hid

    renameExposition :: Rename r => ModuleName -> Exposition SourceRegion Desugared -> Eff r (Exposition SourceRegion Renamed)
    renameExposition mn (ExposedValue vn) = do
        let loc = getLocation vn
        let sr = view sourceRegion loc
        qn <- qualifyIn mn (vn ^. unlocated)
        let varRef = Global (Located sr qn)
        pure $ ExposedValue (TaggedLocate loc varRef)
    renameExposition mn (ExposedOp opn) = do
        let loc = getLocation opn
        let sr = view sourceRegion loc
        qn <- qualifyIn mn (opn ^. unlocated)
        let varRef = Global (Located sr qn)
        pure $ ExposedOp (TaggedLocate loc varRef)
    renameExposition mn (ExposedType tn) = do
        let loc = getLocation tn
        qn <- qualifyIn mn (tn ^. unlocated)
        pure $ ExposedType (TaggedLocate loc qn)
    renameExposition mn (ExposedTypeAndAllConstructors tn) = do
        let loc = getLocation tn
        qn <- qualifyIn mn (tn ^. unlocated)
        pure $ ExposedTypeAndAllConstructors (TaggedLocate loc qn)

    renameImport :: Rename r => Import SourceRegion Desugared -> Eff r (Import SourceRegion Renamed)
    renameImport (Import iloc (Import' name as' qual exp')) = do
        exp'' <- renameExposingOrHiding (name ^. unlocated) exp'
        pure $ Import iloc (Import' name as' qual exp'')

addImportsToContext :: Rename r => [Import SourceRegion Desugared] -> Eff r ()
addImportsToContext = traverse_ addImportToContext

addImportToContext :: Rename r => Import SourceRegion Desugared -> Eff r ()
addImportToContext (Import _ imp) =
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
    Eff r (Module SourceRegion Desugared)
getModuleFromName mn = do
    m <- Rock.fetch (Elara.Query.DesugaredModule (mn ^. unlocated))
    let Module _ m' = m
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
    ImportExposingOrHiding SourceRegion Desugared ->
    Bool ->
    Eff r ()
addModuleToContext mn importSpec qualified = do
    imported <- getModuleFromName mn
    let Module _ importedMod = imported
    let allDecls = importedMod.moduleDeclarations
    let thisMn = mn ^. unlocated

    let declName' = toName . declarationName
    let moduleExported = filter (isExposingAndExists imported . declName') allDecls

    toAdd <- case importSpec of
        ImportExposing ExposingAll ->
            pure moduleExported
        ImportExposing (ExposingSome importList) -> do
            for_ importList $ \expo ->
                unless (any (\d -> isExposition thisMn (declName' d) expo) moduleExported) $
                    throwError $
                        NonExistentModuleDeclaration thisMn (expositionToLocatedName expo)
            pure $ filter (\d -> any (isExposition thisMn (declName' d)) importList) moduleExported
        ImportHiding hidingList -> do
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
declarationName :: Declaration SourceRegion Desugared -> DeclName
declarationName (Declaration _ (Declaration' _ body)) =
    let DeclarationBody _ body' = body
     in case body' of
            ValueDeclaration n _ _ _ _ _ -> DeclVar (n ^. unlocated)
            TypeDeclarationBody n _ _ _ _ _ -> DeclType (n ^. unlocated)
            DeclBodyExtension v -> absurd v

-- | Add a declaration to the renaming state.
addDeclarationToContext ::
    Rename r =>
    Declaration SourceRegion Desugared ->
    Eff r ()
addDeclarationToContext decl@(Declaration _dloc (Declaration' declMN body)) = do
    let DeclarationBody _ body' = body
    let nameLoc :: SourceRegion = case body' of
            ValueDeclaration n _ _ _ _ _ -> view sourceRegion n
            TypeDeclarationBody n _ _ _ _ _ -> view sourceRegion n
            DeclBodyExtension v -> absurd v

    -- create a global var ref for a given name
    -- uses the declaration's module name for qualification
    -- and the var ref's location refers to just the declaration name
    let global :: name -> VarRef name
        global vn =
            let mn = declMN ^. unlocated
             in Global (Qualified vn mn `withLocationOf` nameLoc)

    case declarationName decl of
        DeclVar vn -> Eff.modify @RenameState $ over (the @"varNames") $ insertMerging vn (global vn)
        DeclType tn -> Eff.modify @RenameState $ over (the @"typeNames") $ insertMerging tn (global tn)

    logDebug $ "Added declaration to context: " <> pretty (declarationName decl)

    case body' of
        -- Add all the constructor names to context
        TypeDeclarationBody _ _ (ADT ctors) _ _ _ -> for_ ctors $ \(cn, _) ->
            let tn = cn ^. unlocated
             in Eff.modify @RenameState $ over (the @"typeNames") $ insertMerging tn (global tn)
        _ -> pass

-- | Ensure that a name exists in the context and is exposed
ensureExistsAndExposed :: (Rock.Rock Elara.Query.Query :> r, _) => TaggedLocate ModuleNode SourceRegion ModuleName -> Located Name -> Eff r ()
ensureExistsAndExposed mn n = do
    thisMod <- Eff.ask
    m <- getModuleFromName mn
    unless (elementExistsInModule m (n ^. unlocated)) $ throwError $ NonExistentModuleDeclaration (mn ^. unlocated) n
    unless (isExposingAndExists m (n ^. unlocated)) $ throwError $ UnknownName @Name n thisMod mempty

elementExistsInModule :: Module SourceRegion Desugared -> Name -> Bool
elementExistsInModule (Module _ m') n' =
    any
        ( \decl ->
            toName (declarationName decl) == n'
                || case decl of
                    Declaration _ (Declaration' _ (DeclarationBody _ (TypeDeclarationBody _ _ (ADT ctors) _ _ _))) ->
                        any (\(cn, _) -> NameType (cn ^. unlocated) == n') ctors
                    _ -> False
        )
        m'.moduleDeclarations

{- | Tests that n is exposed in m
I.e. that it is in the exposing list, or that the module is exposing everything
-}
isExposingAndExists :: Module SourceRegion Desugared -> Name -> Bool
isExposingAndExists m@(Module _ m') n =
    let mn = m'.moduleName ^. unlocated
     in case m'.moduleExposing of
            ExposingAll -> elementExistsInModule m n
            ExposingSome es -> elementExistsInModule m n && any (isExposition mn n) es
  where
    isExposition :: ModuleName -> Name -> Exposition SourceRegion Desugared -> Bool
    isExposition mn (NameValue vn) (ExposedValue vn') = MaybeQualified (NormalVarName vn) (Just mn) == vn' ^. unlocated
    isExposition mn (NameOp vn) (ExposedValue vn') = MaybeQualified (OperatorVarName vn) (Just mn) == vn' ^. unlocated
    isExposition mn (NameOp vn) (ExposedOp opn') = MaybeQualified vn (Just mn) == opn' ^. unlocated
    isExposition mn (NameType tn) (ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition mn (NameType tn) (ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition _ _ _ = False

renameDeclaration :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => Declaration SourceRegion Desugared -> Eff r RenamedDeclaration
renameDeclaration (Declaration dloc (Declaration' mn body)) = do
    body' <- renameDeclarationBody body
    pure $ Declaration dloc (Declaration' mn body')
  where
    renameDeclarationBody :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DeclarationBody SourceRegion Desugared -> Eff r RenamedDeclarationBody
    renameDeclarationBody (DeclarationBody bloc body') = DeclarationBody bloc <$> renameDeclarationBody' body'

    renameDeclarationBody' :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DeclarationBody' SourceRegion Desugared -> Eff r RenamedDeclarationBody'
    renameDeclarationBody' (ValueDeclaration name val _pats _mTy mTypeMeta anns) = scoped @RenameState $ do
        mTypeMeta' <- traverse renameSimpleTypeAllowNewVars mTypeMeta
        val' <- renameExpr val
        anns' <- traverse renameAnnotation anns
        thisModule <- askCurrentModule
        let Module _ thisMod = thisModule
        let qualifiedName =
                (\n -> Qualified n (thisMod.moduleName ^. unlocated)) <$> name
        pure $ ValueDeclaration qualifiedName val' () () mTypeMeta' anns'
    renameDeclarationBody' (TypeDeclarationBody name vars typeDecl _mKind _meta anns) = do
        vars' <- traverse uniquify vars
        let varAliases = zip vars vars'
        let addAllVarAliases s =
                foldl'
                    (\s' (vn, uniqueVn) -> the @"typeVars" %~ Map.insert (vn ^. unlocated) (uniqueVn ^. unlocated) $ s')
                    s
                    varAliases
        let declModuleName = mn ^. unlocated
        locally @RenameState addAllVarAliases $ do
            thisModule <- askCurrentModule
            let Module _ thisMod = thisModule
            let qualifiedName =
                    (\n -> Qualified n (thisMod.moduleName ^. unlocated)) <$> name
            typeDecl' <- renameTypeDeclaration declModuleName qualifiedName typeDecl
            anns' <- traverse renameAnnotation anns
            pure $ TypeDeclarationBody qualifiedName vars' typeDecl' Nothing NoExtension anns'
    renameDeclarationBody' (DeclBodyExtension v) = absurd v

renameAnnotation :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => Annotation SourceRegion Desugared -> Eff r (Annotation SourceRegion Renamed)
renameAnnotation (Annotation name args) = do
    name' <- qualifyTypeName name
    args' <- traverse (\(AnnotationArg e) -> AnnotationArg <$> renameExpr e) args
    pure $ Annotation name' args'

renameTypeDeclaration :: _ => ModuleName -> TaggedLocate TypeNode SourceRegion (Qualified TypeName) -> TypeDeclaration SourceRegion Desugared -> Eff r (TypeDeclaration SourceRegion Renamed)
renameTypeDeclaration _ declarationName' (Alias aliasedType) = do
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

    pure $ Alias t'
renameTypeDeclaration thisMod _declarationName' (ADT constructors) = do
    constructors' <-
        traverse
            (\(n, tys) -> (over unlocated (`Qualified` thisMod) n,) <$> traverse renameSimpleType tys)
            constructors
    pure $ ADT constructors'

renameSimpleType :: _ => Type SourceRegion Desugared -> Eff r RenamedType
renameSimpleType = renameSimpleTypeWith False

renameSimpleTypeAllowNewVars :: _ => Type SourceRegion Desugared -> Eff r RenamedType
renameSimpleTypeAllowNewVars = renameSimpleTypeWith True

renameSimpleTypeWith :: _ => Bool -> Type SourceRegion Desugared -> Eff r RenamedType
renameSimpleTypeWith antv (Type loc () t') = do
    t'' <- renameType antv t'
    pure $ Type loc () t''

-- | Renames a type, qualifying type constructors and type variables where necessary
renameType ::
    (InnerRename r, Rock.Rock Elara.Query.Query :> r) =>
    {- | If new type variables are allowed - if 'False', this will throw an error if a type variable is not in scope.
    This is useful for type declarations, where something like @type Invalid a = b@ would clearly be invalid
    But for local type annotations, we want to allow this, as it may be valid to have new type variables there - eg @\x -> (x : a)@
    -}
    Bool ->
    Type' SourceRegion Desugared ->
    Eff r RenamedType'
renameType allowNewTypeVars (TVar (TaggedLocate l n)) = do
    inCtx <- lookupTypeVar n
    case inCtx of
        Just inCtx' -> pure $ TVar (TaggedLocate l inCtx')
        Nothing
            | allowNewTypeVars -> do
                uniqueN <- makeUnique n
                Eff.modify @RenameState $ over (the @"typeVars") $ Map.insert n uniqueN
                pure (TVar $ TaggedLocate l uniqueN)
            | otherwise -> throwError $ UnknownTypeVariable n
renameType antv (TFun t1 t2) = TFun <$> renameSimpleTypeWith antv t1 <*> renameSimpleTypeWith antv t2
renameType _ TUnit = do
    -- turn it into Elara.Prim.()
    let unitTypeName = knownQualified (knownTypeInfo (KnownWiredIn WiredInUnit))
    let unitLoc = wrap TypeNode (generatedSourceRegion Nothing)
    pure $ TUserDefined (TaggedLocate unitLoc unitTypeName)
renameType antv (TApp t1 t2) = TApp <$> renameSimpleTypeWith antv t1 <*> renameSimpleTypeWith antv t2
renameType _ (TUserDefined ln) = TUserDefined <$> qualifyTypeName ln
renameType antv (TRecord fields) = TRecord <$> traverse (traverseOf _2 (renameSimpleTypeWith antv)) fields
renameType antv (TList t) = TList <$> renameSimpleTypeWith antv t
renameType antv (TExtension (TupleType (AtLeast2List fst' snd' []))) = do
    -- turn it into Elara.Prim.Tuple2 type
    fst'' <- renameSimpleTypeWith antv fst'
    snd'' <- renameSimpleTypeWith antv snd'
    let Type fstLoc _ _ = fst'
    let Type sndLoc _ _ = snd'
    let loc = enclosingRegion fstLoc sndLoc
    let tupleCtor = Type loc () (TUserDefined (TaggedLocate loc Prim.tuple2CtorName))
    let base = TApp tupleCtor fst''

    pure $ TApp (Type loc () base) snd''
renameType antv (TExtension (TupleType tupleElems)) = do
    -- turn it into Elara.Prim.TupleN type
    let tupleName = Prim.tupleNCtorName (length tupleElems)

    elems' <- traverse (renameSimpleTypeWith antv) tupleElems
    let loc = foldr1 enclosingRegion (fmap (\(Type l _ _) -> l) elems')
    let tupleCtor = Type loc () (TUserDefined (TaggedLocate loc tupleName))

    let (head', tail) = AtLeast2List.toHeadAndTail elems'
    pure $ foldl' (TApp . Type loc ()) (TApp tupleCtor head') tail

renameExpr :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DesugaredExpr -> Eff r RenamedExpr
renameExpr (Expr loc () e') = Expr loc () <$> renameExpr' e'
  where
    renameExpr' ::
        (InnerRename r, Rock.Rock Elara.Query.Query :> r) =>
        Expr' SourceRegion Desugared -> Eff r RenamedExpr'
    renameExpr' (EInt i) = pure (EInt i)
    renameExpr' (EFloat i) = pure (EFloat i)
    renameExpr' (EString i) = pure (EString i)
    renameExpr' (EChar i) = pure (EChar i)
    renameExpr' EUnit = pure EUnit
    renameExpr' (EVar NoExtension i) = EVar NoExtension <$> lookupVarName i
    renameExpr' (ECon NoExtension i) = ECon NoExtension <$> lookupTypeName i
    renameExpr' (ELam NoExtension pat body) = renameLambda pat body
    renameExpr' (EApp NoExtension e1 e2) = EApp NoExtension <$> renameExpr e1 <*> renameExpr e2
    renameExpr' (ETyApp e1 t1) = ETyApp <$> renameExpr e1 <*> renameSimpleType t1
    renameExpr' (EIf e1 e2 e3) = EIf <$> renameExpr e1 <*> renameExpr e2 <*> renameExpr e3
    renameExpr' (EMatch e cases) = do
        e' <- renameExpr e
        cases' <- traverse (bitraverse renamePattern renameExpr) cases
        pure (EMatch e' cases')
    renameExpr' (ELetIn NoExtension vn e body) = do
        vn' <- uniquify vn
        locally @RenameState (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ Local (stripTag vn'))) $ do
            exp' <- renameExpr e
            body' <- renameExpr body
            pure (ELetIn NoExtension vn' exp' body')
    renameExpr' (EAnn e ty) = EAnn <$> renameExpr e <*> renameSimpleType ty
    renameExpr' (EExtension ext) = renameExprExtension ext

    renameExprExtension :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DesugaredExpressionExtension SourceRegion -> Eff r RenamedExpr'
    renameExprExtension (DesugaredBinaryOperator (BinaryOperatorExpression op left right)) = do
        op' <- renameBinaryOperator op
        left' <- renameExpr left
        right' <- renameExpr right
        pure (EExtension (RenamedBinaryOperator (BinaryOperatorExpression op' left' right')))
    renameExprExtension (DesugaredInParens (InParensExpression e)) = do
        e' <- renameExpr e
        pure (EExtension (RenamedInParens (InParensExpression e')))
    renameExprExtension (DesugaredList (ListExpression [])) = do
        let typeLoc = wrap TypeNode (view sourceRegion loc)
        pure (ECon NoExtension (TaggedLocate typeLoc Prim.nilCtorName))
    renameExprExtension (DesugaredList (ListExpression (x : xs))) = do
        xs' <- traverse renameExpr (x :| xs)
        let lastCons :: RenamedExpr =
                let typeLoc = wrap TypeNode (view sourceRegion loc)
                 in Expr (exprLoc (last xs')) () (ECon NoExtension (TaggedLocate typeLoc Prim.nilCtorName))
        let cons :: RenamedExpr -> RenamedExpr -> RenamedExpr
            cons x' y =
                let xLoc = exprLoc x'
                    yLoc = exprLoc y
                    typeLoc = wrap TypeNode (view sourceRegion loc)
                    consE = Expr loc () (ECon NoExtension (TaggedLocate typeLoc Prim.consCtorName))
                    appConsX = Expr xLoc () (EApp NoExtension consE x')
                 in Expr yLoc () (EApp NoExtension appConsX y)
        let createConses :: [RenamedExpr] -> RenamedExpr
            createConses [] = lastCons
            createConses (x' : xs'') = cons x' (createConses xs'')
        let Expr _ () e'' = createConses (toList xs')
        pure e''
    renameExprExtension (DesugaredTuple (TupleExpression (AtLeast2List fst' snd' []))) = do
        fst'' <- renameExpr fst'
        snd'' <- renameExpr snd'
        let typeLoc = wrap TypeNode (view sourceRegion loc)
        let base =
                Expr
                    loc
                    ()
                    ( EApp
                        NoExtension
                        ( Expr
                            loc
                            ()
                            (ECon NoExtension (TaggedLocate typeLoc Prim.tuple2CtorName))
                        )
                        fst''
                    )
        pure (EApp NoExtension base snd'')
    renameExprExtension (DesugaredTuple (TupleExpression tupleElems)) = do
        -- turn it into Elara.Prim.TupleN type
        let tupleName :: Qualified TypeName = Prim.tupleNCtorName (length tupleElems)

        elems' <- traverse renameExpr tupleElems
        let typeLoc = wrap TypeNode (view sourceRegion loc)
        let headCtorExpr :: Expr SourceRegion Renamed =
                Expr loc () (ECon NoExtension (TaggedLocate typeLoc tupleName))

        let elemsList = AtLeast2List.toNonEmpty elems'
        let Expr _ () r = foldl' (\acc m -> Expr loc () (EApp NoExtension acc m)) headCtorExpr elemsList

        pure r
    renameExprExtension (DesugaredBlock stmts final) = do
        Expr _ () e <- renameBlock stmts final
        pure e
    renameExprExtension (DesugaredLetBangIn name val body) = do
        Expr _ () e <- renameLetBang loc name val (renameExpr body)
        pure e

renameBinaryOperator :: forall r. (InnerRename r, Rock.Rock Elara.Query.Query :> r) => BinaryOperator SourceRegion Desugared -> Eff r (BinaryOperator SourceRegion Renamed)
renameBinaryOperator (SymOp opLoc occ) = do
    TaggedLocate loc op' <- lookupVarName (fmap OperatorVarName <$> occ)
    let onlyOpName (OperatorVarName o') = o'
        onlyOpName _ = error "renameBinaryOperator: I really don't like this"
    let op'' = onlyOpName <$> op'
    pure $ SymOp opLoc (TaggedLocate loc op'')
renameBinaryOperator (InfixedOp opLoc (TaggedLocate l o)) = do
    op' <- case o of
        MaybeQualified (NameValue n) q -> do
            vn <- lookupVarName (TaggedLocate l (MaybeQualified (NormalVarName n) q))
            pure $ withName (vn ^. unlocated)
        MaybeQualified (NameOp n) q -> do
            vn <- lookupVarName (TaggedLocate l (MaybeQualified (OperatorVarName n) q))
            pure $ withName (vn ^. unlocated)
        MaybeQualified (NameType n) q -> do
            let typeLoc = wrap TypeNode (view sourceRegion l)
            tn <- lookupTypeName (TaggedLocate typeLoc (MaybeQualified n q))
            let sr = view sourceRegion l
            let qualifiedName = NameType <$> (tn ^. unlocated)
            pure $ Global (Located sr qualifiedName)
    pure $ InfixedOp opLoc op'

renamePattern :: forall r. (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DesugaredPattern -> Eff r RenamedPattern
renamePattern (Pattern loc meta p') = do
    meta' <- traverse renameSimpleType meta
    p'' <- renamePattern' loc p'
    pure $ Pattern loc meta' p''
  where
    renamePattern' :: NodeLoc PatternNode SourceRegion -> DesugaredPattern' -> Eff r RenamedPattern'
    renamePattern' _ (PInt i) = pure $ PInt i
    renamePattern' _ (PFloat i) = pure $ PFloat i
    renamePattern' _ (PString i) = pure $ PString i
    renamePattern' _ (PChar i) = pure $ PChar i
    renamePattern' _ PWildcard = pure PWildcard
    renamePattern' _ PUnit = pure PUnit
    renamePattern' _ (PVar vn) = do
        vn' <- uniquify vn
        Eff.modify @RenameState (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ Local (stripTag vn')))
        pure $ PVar vn'
    renamePattern' _ (PCon cn ps) = do
        cn' <- qualifyTypeName cn
        ps' <- traverse renamePattern ps
        pure $ PCon cn' ps'
    renamePattern' ploc (PExtension ext) = renamePatternExtension ploc ext

    renamePatternExtension :: NodeLoc PatternNode SourceRegion -> ListTuplePatternExtension SourceRegion Desugared -> Eff r RenamedPattern'
    renamePatternExtension ploc (ListPattern []) = do
        let typeLoc = wrap TypeNode (view sourceRegion ploc)
        pure $ PCon (TaggedLocate typeLoc Prim.nilCtorName) []
    renamePatternExtension ploc (ListPattern (x : xs)) = do
        xs' <- traverse renamePattern (x :| xs)
        let lastCons :: RenamedPattern =
                let typeLoc = wrap TypeNode (view sourceRegion ploc)
                 in Pattern (exprLocP (last xs')) Nothing (PCon (TaggedLocate typeLoc Prim.nilCtorName) [])
        let cons :: RenamedPattern -> RenamedPattern -> RenamedPattern
            cons x' y =
                let typeLoc = wrap TypeNode (view sourceRegion ploc)
                 in Pattern (exprLocP x') Nothing (PCon (TaggedLocate typeLoc Prim.consCtorName) [x', y])
        let createConses :: [RenamedPattern] -> RenamedPattern
            createConses [] = lastCons
            createConses (x' : xs'') = cons x' (createConses xs'')
        let result = createConses (toList xs')
        let Pattern _ _ p'' = result
        pure p''
    renamePatternExtension ploc (ConsPattern p1 p2) = do
        p1' <- renamePattern p1
        p2' <- renamePattern p2
        let typeLoc = tag TypeNode ploc
        pure $ PCon (TaggedLocate typeLoc Prim.consCtorName) [p1', p2']
    renamePatternExtension ploc (TuplePattern (p1 :| [p2])) = do
        p1' <- renamePattern p1
        p2' <- renamePattern p2
        let typeLoc = tag TypeNode ploc
        pure $ PCon (TaggedLocate typeLoc Prim.tuple2CtorName) [p1', p2']
    renamePatternExtension _ (TuplePattern _) = error "renamePattern': TuplePattern more than length 2"

-- | Get location from an expression
exprLoc :: Expr SourceRegion p -> NodeLoc ExprNode SourceRegion
exprLoc (Expr loc _ _) = loc

-- | Get location from a pattern
exprLocP :: Pattern SourceRegion p -> NodeLoc PatternNode SourceRegion
exprLocP (Pattern loc _ _) = loc

{- | Estimates a var name from a pattern
This isn't really necessary as names will be uniquified anyway, but it could make dumped code more readable
-}
patternToVarName :: DesugaredPattern -> VarName
patternToVarName (Pattern _ _ p) =
    let mn = NormalVarName . LowerAlphaName
     in case p of
            PWildcard -> mn "wildcard"
            PVar vn -> vn ^. unlocated
            PInt _ -> mn "int"
            PFloat _ -> mn "float"
            PString _ -> mn "string"
            PChar _ -> mn "char"
            PCon _ _ -> mn "constructor"
            PUnit -> "unit"
            PExtension (ListPattern _) -> mn "list"
            PExtension (ConsPattern _ _) -> mn "cons"
            PExtension (TuplePattern _) -> mn "tuple"

-- | Turn a pattern and a body into a variable and a match expression. Used for renaming lambdas who use patterns as binders.
patternToMatch ::
    (InnerRename r, Rock.Rock Elara.Query.Query :> r) =>
    -- | Pattern to turn into a match
    DesugaredPattern ->
    -- | Body of the lambda
    DesugaredExpr ->
    -- | The variable to bind the match to, and the match expression
    Eff r (TaggedLocate VarNode SourceRegion (Unique VarName), RenamedExpr)
patternToMatch (Pattern _ _ (PVar vn)) body = do
    -- Special case, no match needed
    -- vn :: Located VarName
    uniqueVn <- uniquify vn
    body' <- locally @RenameState (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ (Local :: Located (Unique VarName) -> VarRef VarName) (stripTag uniqueVn))) $ renameExpr body
    pure (uniqueVn, body')
patternToMatch pat body = do
    let vn = patternToVarName pat
    let patLocation = exprLocP pat
    let bodyLocation = exprLoc body
    let varLoc = tag VarNode patLocation
    uniqueVn <- uniquify (TaggedLocate varLoc vn)
    let varRef = Local (stripTag uniqueVn) <$ uniqueVn
    pat' <- renamePattern pat
    body' <- renameExpr body
    let match =
            EMatch
                (Expr (widen (exprLocP pat')) () (EVar NoExtension varRef))
                [(pat', body')]
    pure (uniqueVn, Expr (patLocation <.> bodyLocation) () match)

{- | Rename a lambda expression.
This is a little bit special because patterns have to be converted to match expressions.

For example,
@\(a, b) -> a@  becomes @\ab_ -> match ab_ with (a, b) -> a@
-}
renameLambda :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => DesugaredPattern -> DesugaredExpr -> Eff r RenamedExpr'
renameLambda p@(Pattern _ argType _) e = do
    (arg, match) <- patternToMatch p e
    argType' <- traverse renameSimpleType argType
    let binder = TypedLambdaParam (arg ^. unlocated) argType'
    pure (ELam NoExtension binder match)

{- | Rename a block @{s1; ...; sn; final}@ by desugaring its statements:

* @let x = v; rest@ becomes @let x = v in rest@
* @let! x = v; rest@ becomes @v >>= \x -> rest@
* @e; rest@ becomes @e >>= \ignored -> rest@
-}
renameBlock ::
    (InnerRename r, Rock.Rock Elara.Query.Query :> r) =>
    NonEmpty (Statement SourceRegion Desugared) ->
    DesugaredExpr ->
    Eff r RenamedExpr
renameBlock (Statement l stmt :| rest) final = case stmt of
    SLet n () val -> do
        n' <- uniquify n
        locally @RenameState (the @"varNames" %~ Map.insert (n ^. unlocated) (one $ Local (stripTag n'))) $ do
            val' <- renameExpr val
            Expr l () . ELetIn NoExtension n' val' <$> renameRest
    SLetBang name val -> renameLetBang l name val renameRest
    SExpr x -> do
        x' <- renameExpr x
        ignored <- uniquify (TaggedLocate (tag VarNode (exprLoc x)) (NormalVarName "$ignored"))
        mkBind (exprLoc x') ignored x' <$> renameRest
  where
    renameRest = maybe (renameExpr final) (`renameBlock` final) (nonEmpty rest)

-- | @mkBind loc x val body@ builds @val >>= \x -> body@.
mkBind ::
    -- | location of the statement that produced the bind, which will be used as the location of the @>>=@ reference and its partial application. The overall expression has a span from this location to the end of @body@'s location
    NodeLoc ExprNode SourceRegion ->
    TaggedLocate VarNode SourceRegion (Unique VarName) ->
    RenamedExpr ->
    RenamedExpr ->
    RenamedExpr
mkBind loc binder val body =
    Expr (loc <.> exprLoc body) () (EApp NoExtension partial lam)
  where
    sr = loc ^. sourceRegion
    bindVar = Expr loc () (EVar NoExtension (tagLocated (Located sr (Global (Located sr Prim.bindName)))))
    partial = Expr loc () (EApp NoExtension bindVar val)
    lam = Expr (exprLoc body) () (ELam NoExtension (TypedLambdaParam (stripTag binder ^. unlocated) Nothing) body)

renameLetBang ::
    (InnerRename r, Rock.Rock Elara.Query.Query :> r) =>
    NodeLoc ExprNode SourceRegion ->
    TaggedLocate VarNode SourceRegion VarName ->
    DesugaredExpr ->
    Eff r RenamedExpr -> -- the action to rename the body, executed with the binder in scope
    Eff r RenamedExpr
renameLetBang loc name val renameBody = do
    val' <- renameExpr val -- outside the binder's scope
    name' <- uniquify name
    body' <- locally @RenameState (the @"varNames" %~ Map.insert (name ^. unlocated) (one $ Local (stripTag name'))) renameBody
    pure (mkBind loc name' val' body')

-- | Checks if a type is recursive with respect to a target type, returning the use of the target type if so
typeIsRecursive :: Qualified TypeName -> RenamedType -> Maybe (Located (Qualified TypeName))
typeIsRecursive targetType (Type _loc () t) = case t of
    TVar _ -> Nothing
    TFun a b -> typeIsRecursive targetType a <|> typeIsRecursive targetType b
    TUnit -> Nothing
    TApp a b -> typeIsRecursive targetType a <|> typeIsRecursive targetType b
    TUserDefined locatedName ->
        if locatedName ^. unlocated == targetType
            then Just (stripTag locatedName)
            else Nothing
    TRecord fields -> asum (fmap (typeIsRecursive targetType . snd) fields)
    TList t' -> typeIsRecursive targetType t'
    TExtension v -> absurd v
