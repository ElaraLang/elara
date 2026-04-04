{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}

{- | Renaming stage of compilation
This stage handles:
1. Renaming all variables, types, and type variables, adding module qualification or unique suffixes to avoid name clashes
2. Desugaring any "first-class" pattern matches into normal match expressions (eg '\[] -> 1' to '\x -> match x with [] -> 1')
3. Desugaring blocks into let-in chains (and monad operations soon), eg 'let y = 1; y + 1' to 'let y = 1 in y + 1'
  Note that until the monad operations are implemented, we can't fully remove blocks, as we have nothing to translate 'f x; g x' into
-}
module Elara.Rename (getRenamedModule, renameExpr, InnerRename) where

import Data.Generics.Product hiding (list)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Effectful (Eff, inject, (:>))
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as Eff
import Effectful.Reader.Static qualified as Eff
import Effectful.State.Extra
import Effectful.State.Static.Local qualified as Eff

-- New AST types (primary)
import Elara.AST.Extensions
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name (LowerAlphaName (..), MaybeQualified (MaybeQualified), ModuleName (..), Name (NTypeName, NVarName), OpName (..), Qualified (Qualified), ToName (toName), TypeName (..), VarName (NormalVarName, OperatorVarName), VarOrConName (..))
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Desugared (DesugaredExpressionExtension (..))
import Elara.AST.Phases.Desugared qualified as NewD
import Elara.AST.Phases.Renamed (RenamedExpressionExtension (..), TypedLambdaParam (..))
import Elara.AST.Phases.Renamed qualified as NewR
import Elara.AST.Region (Located (Located), SourceRegion (..), enclosingRegion', generatedSourceRegion, sourceRegion, spanningRegion', unlocated, withLocationOf)
import Elara.AST.Types qualified as New
import Elara.AST.VarRef (VarRef, VarRef' (Global, Local))
import Elara.Data.AtLeast2List (AtLeast2List (AtLeast2List))
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.Data.Unique.Effect
import Elara.Desugar.Error (DesugarError)
import Elara.Error (runErrorOrReport)
import Elara.Logging (StructuredDebug, logDebug)
import Elara.Prim (mkPrimQual, unitName)
import Elara.Prim.Core (consCtorName, emptyListCtorName, tuple2CtorName)
import Elara.Prim.Rename (primitiveRenameState)
import Elara.Query (QueryType (..), SupportsQuery (..))
import Elara.Query qualified
import Elara.Query.Effects
import Elara.Query.Errors (StandardQueryError)
import Elara.Rename.Error
import Elara.Rename.Imports (expositionToLocatedName, isExposition, isImportedBy)
import Elara.Rename.State
import Print (showColored)
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

-- | Run the @'Elara.Query.RenamedModule'@ query, renaming a module by its name
getRenamedModule ::
    ModuleName ->
    Eff
        (ConsQueryEffects '[Eff.Error RenameError, Eff.State RenameState, Rock.Rock Elara.Query.Query])
        (NewModule.Module SourceRegion NewR.Renamed)
getRenamedModule mn = do
    m <- runErrorOrReport @DesugarError $ Rock.fetch $ Elara.Query.DesugaredModule mn
    let NewModule.Module _ m' = m
    let actualName = m'.moduleName ^. unlocated
    when (actualName /= mn) $ throwError $ ModuleNameMismatch (Located (GeneratedRegion "Renaming Entry Point") mn) (actualName `withLocationOf` m'.moduleName)
    rename m

instance SupportsQuery QueryModuleByName NewR.Renamed where
    type QuerySpecificEffectsOf QueryModuleByName NewR.Renamed = '[Eff.Error RenameError]
    query mn =
        Eff.evalState primitiveRenameState $
            inject $
                getRenamedModule mn

qualifyIn :: Rename r => ModuleName -> MaybeQualified name -> Eff r (Qualified name)
qualifyIn mn (MaybeQualified n (Just m)) = do
    when (m /= mn) $ throwError $ QualifiedInWrongModule m mn
    pure $ Qualified n m
qualifyIn mn (MaybeQualified n Nothing) = pure $ Qualified n mn

qualifyTypeName :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => Located (MaybeQualified TypeName) -> Eff r (Located (Qualified TypeName))
qualifyTypeName (Located sr (MaybeQualified n (Just m))) = do
    ensureExistsAndExposed (Located sr m) (Located sr (NTypeName n))
    pure $ Located sr (Qualified n m)
qualifyTypeName (Located sr (MaybeQualified n Nothing)) = do
    typeNames' <- use' (field' @"typeNames")
    m <- Eff.ask
    case Map.lookup n typeNames' of
        Nothing -> throwError $ UnknownName (Located sr (NTypeName n)) m typeNames'
        Just ((Global (Located sr' (Qualified n' m'))) :| []) -> pure $ Located sr' (Qualified n' m')
        Just ((Local _) :| []) -> error "can't have local type names"
        Just many -> throwError $ AmbiguousTypeName (Located sr (NTypeName n)) many

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
    Located (MaybeQualified name) ->
    Eff r (Located (VarRef name))
lookupGenericName _ _ (Located sr (MaybeQualified n (Just m))) = do
    ensureExistsAndExposed (Located sr m) (Located sr (toName n))
    pure $ Located sr $ Global (Located sr (Qualified n m))
lookupGenericName lens ambiguousError (Located sr (MaybeQualified n Nothing)) = do
    names' <- use' lens
    m <- Eff.ask
    case m of
        Nothing ->
            case Map.lookup n names' of
                Nothing -> throwError $ UnknownName (Located sr $ toName n) m names'
                Just (v :| []) -> pure $ Located sr v
                Just many -> throwError $ ambiguousError (Located sr $ toName n) many
        Just m' -> case maybe [] (NonEmpty.filter ((m' `isImportedBy`) . fmap toName)) (Map.lookup n names') of
            [v] -> pure $ Located sr v
            [] -> throwError $ UnknownName (Located sr $ toName n) (Just m') names'
            (x : xs) -> throwError $ ambiguousError (Located sr $ toName n) (x :| xs)

lookupVarName :: _ => Located (MaybeQualified VarName) -> Eff r (Located (VarRef VarName))
lookupVarName = lookupGenericName (field' @"varNames") AmbiguousVarName

lookupTypeName :: (InnerRename r, Rock.Rock Elara.Query.Query :> r) => Located (MaybeQualified TypeName) -> Eff r (Located (Qualified TypeName))
lookupTypeName n =
    lookupGenericName (field' @"typeNames") AmbiguousTypeName n <<&>> \case
        Local _ -> error "can't have local type names"
        Global v -> v ^. unlocated

lookupTypeVar :: _ => LowerAlphaName -> Eff r (Maybe (Unique LowerAlphaName))
lookupTypeVar n = do
    typeVars' <- use' (field' @"typeVars")
    pure $ Map.lookup n typeVars'

uniquify :: UniqueGen :> r => Located name -> Eff r (Located (Unique name))
uniquify (Located sr n) = Located sr <$> makeUnique n

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
        qn <- traverse (qualifyIn mn) vn
        pure $ NewModule.ExposedValue (fmap (\q -> Global (q `withLocationOf` qn)) qn)
    renameExposition mn (NewModule.ExposedOp opn) = do
        qn <- traverse (qualifyIn mn) opn
        let toVarRef q = Global (q `withLocationOf` qn)
        pure $ NewModule.ExposedOp (fmap toVarRef qn)
    renameExposition mn (NewModule.ExposedType tn) =
        NewModule.ExposedType <$> traverse (qualifyIn mn) tn
    renameExposition mn (NewModule.ExposedTypeAndAllConstructors tn) =
        NewModule.ExposedTypeAndAllConstructors <$> traverse (qualifyIn mn) tn

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

getModuleFromName :: Rename r => Located ModuleName -> Eff r (NewModule.Module SourceRegion NewD.Desugared)
getModuleFromName mn = do
    m <-
        runErrorOrReport @DesugarError $
            Rock.fetch (Elara.Query.DesugaredModule (mn ^. unlocated))
    let NewModule.Module _ m' = m
    let actualName = m'.moduleName ^. unlocated
    when (actualName /= mn ^. unlocated) $
        throwError $
            ModuleNameMismatch
                mn
                (actualName `withLocationOf` m'.moduleName)
    pure m

{- | Add all exposed declarations from a module to the renaming context.
This is used when we import a module, adding all the imported names to the context so we can resolve them when we see them in the code.
-}
addModuleToContext ::
    Rename r =>
    Located ModuleName ->
    NewModule.ImportExposingOrHiding SourceRegion NewD.Desugared ->
    Bool ->
    Eff r ()
addModuleToContext mn importSpec qualified = do
    imported <- getModuleFromName mn
    let NewModule.Module _ importedMod = imported
    let allDecls = importedMod.moduleDeclarations
    let thisMn = mn ^. unlocated

    -- Phase 1: what the imported module itself publicly exports
    let moduleExported = filter (isExposingAndExists imported . declarationName) allDecls

    -- Phase 2: apply the import clause against the module's exports, with validation
    toAdd <- case importSpec of
        NewModule.ImportExposing NewModule.ExposingAll ->
            pure moduleExported
        NewModule.ImportExposing (NewModule.ExposingSome importList) -> do
            for_ importList $ \expo ->
                unless (any (\d -> isExposition thisMn (declarationName d) expo) moduleExported) $
                    throwError $
                        NonExistentModuleDeclaration thisMn (expositionToLocatedName expo)
            pure $ filter (\d -> any (isExposition thisMn (declarationName d)) importList) moduleExported
        NewModule.ImportHiding hidingList -> do
            for_ hidingList $ \expo ->
                unless (any (\d -> isExposition thisMn (declarationName d) expo) moduleExported) $
                    throwError $
                        NonExistentModuleDeclaration thisMn (expositionToLocatedName expo)
            pure $ filter (\d -> not $ any (isExposition thisMn (declarationName d)) hidingList) moduleExported

    unless qualified $
        traverse_ addDeclarationToContext toAdd

    when qualified $ do
        let (ModuleName parts) = thisMn
        let simpleModuleName = last parts
        let isPrincipalType decl =
                case declarationName decl of
                    NTypeName tn -> tn == TypeName simpleModuleName
                    _ -> False
        traverse_ addDeclarationToContext (filter isPrincipalType toAdd)

-- | Get the name of a declaration
declarationName :: New.Declaration SourceRegion NewD.Desugared -> Name
declarationName (New.Declaration _ (New.Declaration' _ body)) =
    let New.DeclarationBody _ body' = body
     in case body' of
            New.ValueDeclaration n _ _ _ _ _ -> NVarName (n ^. unlocated)
            New.TypeDeclarationBody n _ _ _ _ _ -> NTypeName (n ^. unlocated)
            New.DeclBodyExtension v -> absurd v

-- | Add a declaration to the renaming state.
addDeclarationToContext ::
    Rename r =>
    New.Declaration SourceRegion NewD.Desugared ->
    Eff r ()
addDeclarationToContext decl@(New.Declaration _ (New.Declaration' declMN body)) = do
    let global :: name -> VarRef name
        global vn =
            let mn = declMN ^. unlocated
             in Global (Qualified vn mn `withLocationOf` declMN)
    case declarationName decl of
        NVarName vn -> Eff.modify $ over (the @"varNames") $ insertMerging vn (global vn)
        NTypeName vn -> Eff.modify $ over (the @"typeNames") $ insertMerging vn (global vn)

    logDebug $ "Added declaration to context: " <> pretty (declarationName decl)

    let New.DeclarationBody _ body' = body
    case body' of
        -- Add all the constructor names to context
        New.TypeDeclarationBody _ _ (New.ADT ctors) _ _ _ -> for_ ctors $ \(cn, _) ->
            let tn = cn ^. unlocated
             in Eff.modify $ over (the @"typeNames") $ insertMerging tn (global tn)
        _ -> pass

-- | Ensure that a name exists in the context and is exposed
ensureExistsAndExposed :: (Rock.Rock Elara.Query.Query :> r, _) => Located ModuleName -> Located Name -> Eff r ()
ensureExistsAndExposed mn n = do
    thisMod <- Eff.ask
    m <- getModuleFromName mn
    unless (elementExistsInModule m (n ^. unlocated)) $ throwError $ NonExistentModuleDeclaration (mn ^. unlocated) n
    unless (isExposingAndExists m (n ^. unlocated)) $ throwError $ UnknownName @Name n thisMod mempty

elementExistsInModule :: NewModule.Module SourceRegion NewD.Desugared -> Name -> Bool
elementExistsInModule (NewModule.Module _ m') n' =
    any
        ( \decl ->
            declarationName decl == n'
                || case decl of
                    New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ (New.TypeDeclarationBody _ _ (New.ADT ctors) _ _ _))) ->
                        any (\(cn, _) -> NTypeName (cn ^. unlocated) == n') ctors
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
    isExposition mn (NVarName vn) (NewModule.ExposedValue vn') = MaybeQualified vn (Just mn) == vn' ^. unlocated
    isExposition mn (NTypeName tn) (NewModule.ExposedType tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
    isExposition mn (NTypeName tn) (NewModule.ExposedTypeAndAllConstructors tn') = MaybeQualified tn (Just mn) == tn' ^. unlocated
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
                sequenceA $
                    Qualified name (thisMod.moduleName ^. unlocated)
        pure $ New.ValueDeclaration qualifiedName val' () () mTypeMeta' anns'
    renameDeclarationBody' (New.TypeDeclarationBody name vars typeDecl _mKind _meta anns) = do
        vars' <- traverse uniquify vars
        let varAliases = zip vars vars' :: [(Located LowerAlphaName, Located (Unique LowerAlphaName))]
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
                    sequenceA $
                        Qualified name (thisMod.moduleName ^. unlocated)
            typeDecl' <- renameTypeDeclaration declModuleName qualifiedName typeDecl
            anns' <- traverse renameAnnotation anns
            pure $ New.TypeDeclarationBody qualifiedName vars' typeDecl' Nothing NoExtension anns'
    renameDeclarationBody' (New.DeclBodyExtension v) = absurd v

renameAnnotation :: (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) => New.Annotation SourceRegion NewD.Desugared -> Eff r (New.Annotation SourceRegion NewR.Renamed)
renameAnnotation (New.Annotation name args) = do
    name' <- qualifyTypeName name
    args' <- traverse (\(New.AnnotationArg e) -> New.AnnotationArg <$> renameExpr e) args
    pure $ New.Annotation name' args'

renameTypeDeclaration :: _ => ModuleName -> Located (Qualified TypeName) -> New.TypeDeclaration SourceRegion NewD.Desugared -> Eff r (New.TypeDeclaration SourceRegion NewR.Renamed)
renameTypeDeclaration _ declarationName' (New.Alias aliasedType) = do
    t' <- renameSimpleType aliasedType
    let isRecursive = typeIsRecursive (declarationName' ^. unlocated) t'
    whenJust isRecursive $ \r -> do
        logDebug
            ( "Detected recursive type alias: "
                <> pretty (showColored r)
                <> " at "
                <> pretty (r ^. sourceRegion)
            )
        throwError $ RecursiveTypeAlias declarationName' r

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
renameType allowNewTypeVars (New.TVar (Located sr n)) = do
    inCtx <- lookupTypeVar n
    case inCtx of
        Just inCtx' -> pure $ New.TVar (Located sr inCtx')
        Nothing
            | allowNewTypeVars -> do
                uniqueN <- makeUnique n
                Eff.modify $ over (the @"typeVars") $ Map.insert n uniqueN
                pure (New.TVar $ Located sr uniqueN)
            | otherwise -> throwError $ UnknownTypeVariable n
renameType antv (New.TFun t1 t2) = New.TFun <$> renameSimpleTypeWith antv t1 <*> renameSimpleTypeWith antv t2
renameType _ New.TUnit = do
    -- turn it into Elara.Prim.()
    let unitTypeName = mkPrimQual unitName
    pure $ New.TUserDefined (Located (generatedSourceRegion Nothing) unitTypeName)
renameType antv (New.TApp t1 t2) = New.TApp <$> renameSimpleTypeWith antv t1 <*> renameSimpleTypeWith antv t2
renameType _ (New.TUserDefined ln) = New.TUserDefined <$> qualifyTypeName ln
renameType antv (New.TRecord fields) = New.TRecord <$> traverse (traverseOf _2 (renameSimpleTypeWith antv)) fields
renameType antv (New.TList t) = New.TList <$> renameSimpleTypeWith antv t
renameType antv (New.TExtension (TupleType (AtLeast2List fst' snd' []))) = do
    -- turn it into Elara.Prim.Tuple2 type
    fst'' <- renameSimpleTypeWith antv fst'
    snd'' <- renameSimpleTypeWith antv snd'
    let tupleCtorName = TypeName <$> tuple2CtorName
    let New.Type fstLoc _ _ = fst'
    let New.Type sndLoc _ _ = snd'
    let loc = enclosingRegion' fstLoc sndLoc
    let tupleCtor = New.Type loc () (New.TUserDefined (Located loc tupleCtorName))
    let base = New.TApp tupleCtor fst''

    pure $ New.TApp (New.Type loc () base) snd''
renameType _ (New.TExtension (TupleType{})) = error "renameType: Tuple more than length 2"

renameExpr :: (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) => NewD.DesugaredExpr -> Eff r NewR.RenamedExpr
renameExpr (New.Expr _ () (New.EBlock es)) = desugarBlock es
renameExpr e@(New.Expr _ () (New.ELet{})) = desugarBlock (e :| [])
renameExpr (New.Expr loc () e') = do
    (e'', meta) <- renameExpr' loc e'
    pure $ New.Expr loc meta e''
  where
    renameExpr' :: (InnerRename r, Eff.Reader (Maybe (New.Declaration SourceRegion NewD.Desugared)) :> r, Rock.Rock Elara.Query.Query :> r) => SourceRegion -> New.Expr' SourceRegion NewD.Desugared -> Eff r (NewR.RenamedExpr', Maybe NewR.RenamedType)
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
        locally (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ (Local :: Located (Unique VarName) -> VarRef VarName) vn')) $ do
            exp' <- renameExpr e
            body' <- renameExpr body
            pure (New.ELetIn NoExtension vn' exp' body', Nothing)
    renameExpr' _ (New.EAnn e ty) = do
        e' <- renameExpr e
        ty' <- renameSimpleType ty
        let New.Expr eloc _ e'' = e'
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
    renameExprExtension (DesugaredList (ListExpression [])) = pure (New.ECon NoExtension (Located loc emptyListCtorName), Nothing)
    renameExprExtension (DesugaredList (ListExpression (x : xs))) = do
        xs' <- traverse renameExpr (x :| xs)
        let lastCons :: NewR.RenamedExpr =
                New.Expr (exprLoc (last xs')) Nothing (New.ECon NoExtension (Located loc emptyListCtorName))
        let cons :: NewR.RenamedExpr -> NewR.RenamedExpr -> NewR.RenamedExpr
            cons x' y =
                let xLoc = exprLoc x'
                    yLoc = exprLoc y
                    consE = New.Expr loc Nothing (New.ECon NoExtension (Located loc consCtorName))
                    appConsX = New.Expr xLoc Nothing (New.EApp NoExtension consE x')
                 in New.Expr yLoc Nothing (New.EApp NoExtension appConsX y)
        let createConses :: [NewR.RenamedExpr] -> NewR.RenamedExpr
            createConses [] = lastCons
            createConses (x' : xs'') = cons x' (createConses xs'')
        let result = createConses (toList xs')
        let New.Expr _ meta e'' = result
        pure (e'', meta)
    renameExprExtension (DesugaredTuple (TupleExpression (AtLeast2List fst' snd' []))) = do
        let tupleCtorName = TypeName <$> tuple2CtorName
        fst'' <- renameExpr fst'
        snd'' <- renameExpr snd'
        let base = New.Expr loc Nothing (New.EApp NoExtension (New.Expr loc Nothing (New.ECon NoExtension (Located loc tupleCtorName))) fst'')
        pure (New.EApp NoExtension base snd'', Nothing)
    renameExprExtension (DesugaredTuple _) = error "renameExpr': Tuple more than length 2"

renameBinaryOperator :: forall r. (InnerRename r, Rock.Rock Elara.Query.Query :> r) => New.BinaryOperator SourceRegion NewD.Desugared -> Eff r (New.BinaryOperator SourceRegion NewR.Renamed)
renameBinaryOperator (New.SymOp opLoc occ) = do
    op' <- lookupVarName (OperatorVarName <<$>> occ)
    let onlyOpName (OperatorVarName o') = o'
        onlyOpName _ = error "renameBinaryOperator: I really don't like this"
    let op'' = onlyOpName <<$>> op'
    pure $ New.SymOp opLoc op''
renameBinaryOperator (New.InfixedOp opLoc (Located l o)) = do
    op' :: VarRef VarOrConName <- case o of
        MaybeQualified (VarName n) q -> do
            vn <- lookupVarName (Located l (MaybeQualified (NormalVarName n) q))
            let onlyVarName (NormalVarName n') = n'
                onlyVarName _ = error "renameBinaryOperator: I really don't like this"
            pure $ (VarName . onlyVarName <<$>> vn) ^. unlocated
        MaybeQualified (ConName n) q -> do
            tn <- lookupTypeName (Located l (MaybeQualified n q))
            pure $ Global (ConName <<$>> tn)
    pure $ New.InfixedOp opLoc op'

renamePattern :: forall r. (InnerRename r, Rock.Rock Elara.Query.Query :> r) => NewD.DesugaredPattern -> Eff r NewR.RenamedPattern
renamePattern (New.Pattern loc meta p') = do
    meta' <- traverse renameSimpleType meta
    p'' <- renamePattern' loc p'
    pure $ New.Pattern loc meta' p''
  where
    renamePattern' :: SourceRegion -> NewD.DesugaredPattern' -> Eff r NewR.RenamedPattern'
    renamePattern' _ (New.PInt i) = pure $ New.PInt i
    renamePattern' _ (New.PFloat i) = pure $ New.PFloat i
    renamePattern' _ (New.PString i) = pure $ New.PString i
    renamePattern' _ (New.PChar i) = pure $ New.PChar i
    renamePattern' _ New.PWildcard = pure New.PWildcard
    renamePattern' _ New.PUnit = pure New.PUnit
    renamePattern' _ (New.PVar vn) = do
        -- vn :: Located VarName
        vn' <- uniquify vn -- vn' :: Located (Unique VarName)
        Eff.modify (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ (Local :: Located (Unique VarName) -> VarRef VarName) vn'))
        pure $ New.PVar vn'
    renamePattern' ploc (New.PCon cn ps) = do
        cn' <- qualifyTypeName cn
        ps' <- traverse renamePattern ps
        pure $ New.PCon cn' ps'
    renamePattern' ploc (New.PExtension ext) = renamePatternExtension ploc ext

    renamePatternExtension :: SourceRegion -> ListTuplePatternExtension SourceRegion NewD.Desugared -> Eff r NewR.RenamedPattern'
    renamePatternExtension ploc (ListPattern []) = pure $ New.PCon (Located ploc emptyListCtorName) []
    renamePatternExtension ploc (ListPattern (x : xs)) = do
        xs' <- traverse renamePattern (x :| xs)
        let lastCons :: NewR.RenamedPattern =
                New.Pattern (exprLocP (last xs')) Nothing (New.PCon (Located ploc emptyListCtorName) [])
        let cons :: NewR.RenamedPattern -> NewR.RenamedPattern -> NewR.RenamedPattern
            cons x' y = New.Pattern (exprLocP x') Nothing (New.PCon (Located ploc consCtorName) [x', y])
        let createConses :: [NewR.RenamedPattern] -> NewR.RenamedPattern
            createConses [] = lastCons
            createConses (x' : xs'') = cons x' (createConses xs'')
        let result = createConses (toList xs')
        let New.Pattern _ _ p'' = result
        pure p''
    renamePatternExtension ploc (ConsPattern p1 p2) = do
        p1' <- renamePattern p1
        p2' <- renamePattern p2
        pure $ New.PCon (Located ploc consCtorName) [p1', p2']
    renamePatternExtension ploc (TuplePattern (p1 :| [p2])) = do
        let tupleCtorName = TypeName <$> tuple2CtorName
        p1' <- renamePattern p1
        p2' <- renamePattern p2
        pure $ New.PCon (Located ploc tupleCtorName) [p1', p2']
    renamePatternExtension _ (TuplePattern _) = error "renamePattern': TuplePattern more than length 2"

-- | Get location from an expression
exprLoc :: New.Expr SourceRegion p -> SourceRegion
exprLoc (New.Expr loc _ _) = loc

-- | Get location from a pattern
exprLocP :: New.Pattern SourceRegion p -> SourceRegion
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
    Eff r (Located (Unique VarName), NewR.RenamedExpr)
patternToMatch (New.Pattern _ _ (New.PVar vn)) body = do
    -- Special case, no match needed
    -- vn :: Located VarName
    uniqueVn <- uniquify vn
    body' <- locally (the @"varNames" %~ Map.insert (vn ^. unlocated) (one $ (Local :: Located (Unique VarName) -> VarRef VarName) uniqueVn)) $ renameExpr body
    pure (uniqueVn, body')
patternToMatch pat body = do
    let vn = patternToVarName pat
    let patLocation = exprLocP pat
    let bodyLocation = exprLoc body
    uniqueVn <- uniquify (Located patLocation vn)
    let varRef = Local uniqueVn `withLocationOf` uniqueVn
    pat' <- renamePattern pat
    body' <- renameExpr body
    let match =
            New.EMatch
                (New.Expr (exprLocP pat') Nothing (New.EVar NoExtension varRef))
                [(pat', body')]
    pure (uniqueVn, New.Expr (enclosingRegion' patLocation bodyLocation) Nothing match)

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
    locally (the @"varNames" %~ Map.insert (n ^. unlocated) (one $ Local n')) $ do
        val' <- renameExpr val
        block <- desugarBlock (xs1 :| xs')
        pure $ New.Expr l Nothing (New.ELetIn NoExtension n' val' block)
desugarBlock xs = do
    let loc = spanningRegion' (xs <&> exprLoc)
    xs' <- traverse renameExpr xs
    pure $ New.Expr loc Nothing (New.EBlock xs')

-- | Checks if a type is recursive with respect to a target type, returning the use of the target type if so
typeIsRecursive :: Qualified TypeName -> NewR.RenamedType -> Maybe (Located (Qualified TypeName))
typeIsRecursive targetType (New.Type _loc () t) = case t of
    New.TVar _ -> Nothing
    New.TFun a b -> typeIsRecursive targetType a <|> typeIsRecursive targetType b
    New.TUnit -> Nothing
    New.TApp a b -> typeIsRecursive targetType a <|> typeIsRecursive targetType b
    New.TUserDefined (Located useSiteLoc n) ->
        if n == targetType
            then Just (Located useSiteLoc n)
            else Nothing
    New.TRecord fields -> asum (fmap (typeIsRecursive targetType . snd) fields)
    New.TList t' -> typeIsRecursive targetType t'
    New.TExtension v -> absurd v
