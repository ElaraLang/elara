{-# LANGUAGE MultiWayIf #-}

-- | Converts typed AST to Core
module Elara.ToCore (runGetCoreModuleQuery, runGetDataConQuery, runGetTyConQuery) where

import Data.Generics.Product (field, field')
import Data.Generics.Sum (AsAny (_As))
import Data.Generics.Wrapped (_Unwrapped)
import Data.Graph (SCC (..), flattenSCC, stronglyConnComp)
import Data.Map qualified as M
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Elara.AST.Module qualified as NewModule
import Elara.AST.Name (ModuleName, Name (..), NameLike (..), Qualified (..), TypeName (..), VarName)
import Elara.AST.Phase (NoExtension (..))
import Elara.AST.Phases.Renamed (TypedLambdaParam (..))
import Elara.AST.Phases.Typed (Typed, TypedDeclaration, TypedExpr, TypedExpr', TypedPattern)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Types qualified as New
import Elara.AST.VarRef (UnlocatedVarRef, VarRef, VarRef' (Global, Local), varRefVal)
import Elara.Core as Core
import Elara.Core.Generic (Bind (..))
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..), CoreTypeDecl (..), CoreTypeDeclBody (CoreDataDecl, CoreTypeAlias))
import Elara.Core.Pretty ()
import Elara.Data.Kind (ElaraKind (..))
import Elara.Data.Pretty (Pretty (..), vcat, (<+>))
import Elara.Data.Unique (Unique)
import Elara.Data.Unique.Effect
import Elara.Error (ReportableError (..), runErrorOrReport, writeReport)
import Elara.Logging
import Elara.Prim (charName, floatName, intName, ioName, mkPrimQual, stringName, unitName)
import Elara.Prim.Core
import Elara.Query qualified
import Elara.Query.Effects (ConsQueryEffects, QueryEffects)
import Elara.ToCore.Match qualified as Match
import Elara.TypeInfer.Type qualified as Type
import Elara.TypeInfer.Unique (UniqueTyVar)
import Elara.Utils (uncurry3)
import Error.Diagnose (Report (..))
import Rock qualified
import TODO (todo)

data ToCoreError
    = LetInTopLevel !TypedExpr
    | UnknownConstructor !(Located (Qualified TypeName)) CtorSymbolTable
    | UnknownPrimConstructor !(Qualified TypeName)
    | UnknownTypeConstructor !(Qualified Text) CtorSymbolTable
    | UnknownLambdaType !(Type.Type SourceRegion)
    | UnsolvedTypeSnuckIn !(Type.Type SourceRegion)
    | UnknownVariable !(Located (Qualified Name))
    deriving (Show)

instance ReportableError ToCoreError where
    report (LetInTopLevel _) = writeReport $ Err (Just "LetInTopLevel") "A top-level let binding was encountered. This is likely a bug in the compiler." [] []
    report (UnknownConstructor (Located _ qn) syms) =
        writeReport $
            Err
                (Just "UnknownDataConstructor")
                ( vcat
                    [ pretty qn
                    , "Known constructors:"
                    , vcat $ pretty <$> M.keys syms.dataCons
                    ]
                )
                []
                []
    report (UnknownPrimConstructor qn) = writeReport $ Err (Just "UnknownPrimConstructor") (pretty qn) [] []
    report (UnknownTypeConstructor qn syms) =
        writeReport $
            Err
                (Just "UnknownTypeConstructor")
                ( vcat
                    [ pretty qn
                    , "Known constructors:"
                    , vcat $ pretty <$> M.keys syms.tyCons
                    ]
                )
                []
                []
    report (UnknownLambdaType t) = writeReport $ Err (Just "UnknownLambdaType") (vcat ["Unknown lambda type:", pretty t]) [] []
    report (UnsolvedTypeSnuckIn t) = do
        writeReport $
            Err
                (Just "UnsolvedTypeSnuckIn")
                (vcat ["An unsolved type snuck into the Core conversion:", pretty t, pretty $ prettyCallStack callStack])
                [ "This is likely a bug in the compiler."
                ]
                []
    report (UnknownVariable (Located _ qn)) = writeReport $ Err (Just "UnknownVariable") (pretty qn) [] []

data CtorSymbolTable = CtorSymbolTable
    { dataCons :: Map (Qualified Text) DataCon
    , tyCons :: Map (Qualified Text) TyCon
    }
    deriving (Show)

primCtorSymbolTable :: CtorSymbolTable
primCtorSymbolTable =
    CtorSymbolTable
        ( fromList
            [ (trueCtorName, trueCtor)
            , (falseCtorName, falseCtor)
            ]
        )
        ( fromList
            [(mkPrimQual "IO", ioCon)]
        )

lookupCtor :: ToCoreC r => Located (Qualified TypeName) -> Eff r DataCon
lookupCtor qn = do
    table <- get @CtorSymbolTable
    let plainName = nameText <$> qn ^. unlocated
    case M.lookup plainName table.dataCons of
        Just ctor -> pure ctor
        Nothing -> Rock.fetch (Elara.Query.GetDataCon (qn ^. unlocated)) ?:! throwError (UnknownConstructor qn table)

registerCtor :: ToCoreC r => DataCon -> Eff r ()
registerCtor ctor = modify (\s -> s{dataCons = M.insert (ctor ^. field @"name") ctor s.dataCons})

registerTyCon :: ToCoreC r => TyCon -> Eff r ()
registerTyCon ctor@(TyCon name _) = modify (\s -> s{tyCons = M.insert name ctor s.tyCons})

lookupPrimCtor :: ToCoreC r => Qualified TypeName -> Eff r DataCon
lookupPrimCtor qn = do
    table <- get @CtorSymbolTable
    case M.lookup (nameText <$> qn) table.dataCons of
        Just ctor -> pure ctor
        Nothing -> Rock.fetch (Elara.Query.GetDataCon qn) ?:! throwError (UnknownPrimConstructor qn)

lookupTyCon :: HasCallStack => ToCoreC r => Qualified Text -> Eff r TyCon
lookupTyCon qn = debugWithResult ("lookupTyCon: " <> pretty qn) $ do
    table <- get @CtorSymbolTable
    case M.lookup qn table.tyCons of
        Just ctor -> pure ctor
        Nothing -> do
            logDebug $ "lookupTyCon failed for: " <> pretty qn
            logDebug $ "Available TyCons: " <> pretty (M.keys table.tyCons)
            Rock.fetch (Elara.Query.GetTyCon qn) ?:! throwError (UnknownTypeConstructor qn table)

runGetDataConQuery ::
    Qualified TypeName -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) (Maybe DataCon)
runGetDataConQuery qn = logDebugWith ("runGetDataConQuery: " <> pretty qn) $ do
    coreModule <- Rock.fetch (Elara.Query.GetCoreModule (qualifier qn))
    let decls = coreModule ^.. field' @"declarations" % each % _As @"CoreType" % field @"typeBody" % _As @"CoreDataDecl" % _2 % each
    let matchingDataCon = find (\(DataCon name _ _) -> name == fmap nameText qn) decls
    pure matchingDataCon

runGetTyConQuery ::
    Qualified Text -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) (Maybe TyCon)
runGetTyConQuery qn
    | qn == mkPrimQual (nameText intName) = pure $ Just intCon
    | qn == mkPrimQual (nameText charName) = pure $ Just charCon
    | qn == mkPrimQual (nameText stringName) = pure $ Just stringCon
    | qn == mkPrimQual (nameText floatName) = pure $ Just floatCon
    | qn == mkPrimQual (nameText unitName) = pure $ Just unitCon
    | qn == mkPrimQual (nameText ioName) = pure $ Just ioCon
    | otherwise = logDebugWith ("runGetTyConQuery: " <> pretty qn) $ do
        let name = NTypeName . TypeName <$> qn
        typedDecl <- Rock.fetch (Elara.Query.TypeCheckedDeclaration name)
        Just
            <$> ( runErrorOrReport @ToCoreError $
                    evalState primCtorSymbolTable $
                        createTyConFromTyped typedDecl
                )

createTyConFromTyped :: InnerToCoreC r => TypedDeclaration -> Eff r TyCon
createTyConFromTyped (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
    case body' of
        New.TypeDeclarationBody n _tvs (New.ADT ctors) _maybeTy _metadata _annotations -> do
            let typeName = nameText <$> (n ^. unlocated)
            let ctorNames = fmap (\(Located _ cn, _) -> fmap nameText cn) (toList ctors)
            pure $ Core.TyCon typeName (TyADT ctorNames)
        New.TypeDeclarationBody n _tvs (New.Alias t) _maybeTy _metadata _annotations -> do
            let typeName = nameText <$> (n ^. unlocated)
            logDebug ("Creating type alias TyCon for: " <> pretty typeName)
            coreType <- astTypeToCore t
            pure $ Core.TyCon typeName (TyAlias coreType)
        body -> error $ "createTyConFromTyped: Expected TypeDeclarationBody but got " <> show body

type ToCoreC r =
    ( State CtorSymbolTable :> r
    , Error ToCoreError :> r
    , UniqueGen :> r
    , StructuredDebug :> r
    , QueryEffects r
    , Rock.Rock Elara.Query.Query :> r
    )

type InnerToCoreC r =
    ( State CtorSymbolTable :> r
    , Error ToCoreError :> r
    , UniqueGen :> r
    , StructuredDebug :> r
    , QueryEffects r
    , Rock.Rock Elara.Query.Query :> r
    )

runGetCoreModuleQuery ::
    ModuleName -> Eff (ConsQueryEffects '[Rock.Rock Elara.Query.Query]) (CoreModule CoreBind)
runGetCoreModuleQuery mn = do
    typedModule <- Rock.fetch (Elara.Query.TypeCheckedModule mn)
    runErrorOrReport @ToCoreError $
        evalState primCtorSymbolTable $
            moduleToCore typedModule

sccToCore :: ToCoreC r => SCC (Qualified VarName, TypedExpr) -> Eff r (CoreDeclaration CoreBind)
sccToCore scc = case scc of
    AcyclicSCC (name, expr) -> do
        debugWith ("sccToCore: AcyclicSCC " <> pretty name) $ do
            expr' <- toCore expr
            let New.Expr _ meta _ = expr
            ty <- typeToCore meta
            let var = Core.Id (Global (nameText <$> name)) ty Nothing
            pure $ CoreValue $ NonRecursive (var, expr')
    CyclicSCC binds -> do
        debugWith ("sccToCore: CyclicSCC " <> pretty (fmap fst binds)) $ do
            binds' <- for binds $ \(name, expr) -> do
                expr' <- toCore expr
                let New.Expr _ meta _ = expr
                ty <- typeToCore meta
                let var = Core.Id (Global (nameText <$> name)) ty Nothing
                pure (var, expr')
            pure $ CoreValue $ Recursive binds'

-- Build module-level SCCs once (no duplicates)
buildModuleSCCs :: ToCoreC r => NewModule.Module SourceRegion Typed -> Eff r [SCC (Qualified VarName)]
buildModuleSCCs (NewModule.Module _ m) = do
    let modName = m.moduleName ^. unlocated
    let tops = mapMaybe extractValueName m.moduleDeclarations
    nodes <- for tops $ \v -> do
        depsAll <- Rock.fetch (Elara.Query.FreeVarsOf v)
        let deps = filter ((== modName) . qualifier) (toList depsAll)
        pure (v, v, deps)
    pure (stronglyConnComp nodes)
  where
    extractValueName :: New.Declaration SourceRegion Typed -> Maybe (Qualified VarName)
    extractValueName (New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) =
        case body' of
            New.ValueDeclaration n _ _ _ _ _ -> Just (n ^. unlocated)
            _ -> Nothing

moduleToCore :: HasCallStack => ToCoreC r => NewModule.Module SourceRegion Typed -> Eff r (CoreModule CoreBind)
moduleToCore m'@(NewModule.Module _ m) = logDebugWith ("Converting module: " <> pretty m.moduleName) $ do
    let name = m.moduleName ^. unlocated

    -- add all tycons first
    for_ m.moduleDeclarations $ \(New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) -> do
        case body' of
            New.TypeDeclarationBody n _tvs (New.ADT ctors) _maybeTy _metadata _annotations -> do
                let tyCon =
                        TyCon
                            (nameText <$> n ^. unlocated)
                            (TyADT (fmap (\(Located _ cn, _) -> fmap nameText cn) (toList ctors)))
                registerTyCon tyCon
            New.TypeDeclarationBody n _tvs (New.Alias t) _maybeTy _metadata _annotations -> do
                tCore <- astTypeToCore t
                registerTyCon (TyCon (nameText <$> n ^. unlocated) (TyAlias tCore))
            _ -> pass

    -- Process declarations (type declarations become CoreType, value declarations handled via SCCs)
    decls <- fmap concat $ for m.moduleDeclarations $ \(New.Declaration _ (New.Declaration' _ (New.DeclarationBody _ body'))) -> do
        case body' of
            New.ValueDeclaration{} -> pure []
            New.TypeDeclarationBody n tvs (New.ADT ctors) _maybeTy kind _annotations -> logDebugWith ("Type decl: " <+> pretty n) $ do
                let cleanedTypeDeclName = nameText <$> (n ^. unlocated)
                let tyCon =
                        TyCon
                            cleanedTypeDeclName
                            ( TyADT
                                (fmap (\(Located _ cn, _) -> fmap nameText cn) (toList ctors))
                            )
                logDebug (pretty tyCon)
                registerTyCon tyCon
                ctors' <- for (toList ctors) $ \(Located _ ctorName, ctorArgs) -> do
                    ctorArgs' <- traverse astTypeToCore ctorArgs
                    let ctorType =
                            foldr
                                (Core.ForAllTy . mkTypeVar . view unlocated)
                                ( foldr
                                    Core.FuncTy
                                    (flipfoldl' (flip Core.AppTy . TyVarTy . mkTypeVar . view unlocated) (ConTy tyCon) tvs)
                                    ctorArgs'
                                )
                                tvs
                    debug $ "Ctor: " <+> pretty ctorName <+> " with type: " <+> pretty ctorType
                    pure (nameText <$> ctorName, ctorType, tyCon)
                let ctors'' = fmap (uncurry3 DataCon) ctors'
                traverse_ registerCtor ctors''
                pure
                    [ CoreType $
                        CoreTypeDecl
                            cleanedTypeDeclName
                            kind
                            (fmap (mkTypeVar . view unlocated) tvs)
                            (CoreDataDecl tyCon (toList ctors''))
                    ]
            New.TypeDeclarationBody n tvs (New.Alias t) _maybeTy kind _annotations -> do
                let cleanedTypeDeclName = nameText <$> (n ^. unlocated)
                t' <- astTypeToCore t
                let tyCon = TyCon cleanedTypeDeclName (TyAlias t')
                registerTyCon tyCon
                pure
                    [ CoreType $
                        CoreTypeDecl
                            cleanedTypeDeclName
                            kind
                            (fmap (mkTypeVar . view unlocated) tvs)
                            (CoreTypeAlias t')
                    ]
            New.DeclBodyExtension v -> absurd v

    sccs <- buildModuleSCCs m'
    logDebug ("Module SCCs: " <> pretty (fmap flattenSCC sccs))
    valueDecls <- for sccs $ \scc -> do
        members <- for scc $ \n -> (n,) <$> Rock.fetch (Elara.Query.TypeCheckedExpr n)
        sccToCore members
    pure $ CoreModule name (decls <> valueDecls)

mkTypeVar :: UniqueTyVar -> Core.TypeVariable
mkTypeVar tv = TypeVariable tv TypeKind

polytypeToCore :: HasCallStack => InnerToCoreC r => Type.Polytype SourceRegion -> Eff r Core.Type
polytypeToCore (Type.Forall _ tvs _constraints t) = do
    t' <- typeToCore t
    let tvs' = fmap mkTypeVar tvs
    pure $ foldr Core.ForAllTy t' tvs'

eitherTypeToCore :: HasCallStack => InnerToCoreC r => Type.Type SourceRegion -> Eff r Core.Type
eitherTypeToCore (Type.Polytype p) = polytypeToCore p
eitherTypeToCore (Type.Lifted t) = typeToCore t

typeToCore :: HasCallStack => InnerToCoreC r => Type.Monotype SourceRegion -> Eff r Core.Type
typeToCore (Type.TypeVar _ (Type.SkolemVar v)) = pure $ Core.TyVarTy $ TypeVariable v TypeKind
typeToCore (Type.TypeVar _ (Type.UnificationVar v)) = pure $ Core.TyVarTy $ TypeVariable v TypeKind
typeToCore (Type.Function _ t1 t2) = Core.FuncTy <$> typeToCore t1 <*> typeToCore t2
typeToCore (Type.TypeConstructor _ qn ts) = do
    let name = fmap (view _Unwrapped) qn
    ts' <- traverse typeToCore ts

    if
        | name == mkPrimQual (nameText intName) -> pure $ foldl' Core.AppTy (Core.ConTy intCon) ts'
        | name == mkPrimQual (nameText charName) -> pure $ foldl' Core.AppTy (Core.ConTy charCon) ts'
        | name == mkPrimQual (nameText stringName) -> pure $ foldl' Core.AppTy (Core.ConTy stringCon) ts'
        | name == mkPrimQual (nameText unitName) -> pure $ foldl' Core.AppTy (Core.ConTy unitCon) ts'
        | name == mkPrimQual (nameText ioName) -> pure $ foldl' Core.AppTy (Core.ConTy ioCon) ts'
        | otherwise -> debugWith ("Type constructor: " <+> pretty qn <+> " with args: " <+> pretty ts) $ do
            tyCon <- lookupTyCon name
            pure $ foldl' Core.AppTy (Core.ConTy tyCon) ts'

{- | Convert an AST-level type (from the Typed phase) to a Core type.
At the Typed phase, TypeMeta is ElaraKind and TypeVariable is Located UniqueTyVar.
-}
astTypeToCore :: HasCallStack => InnerToCoreC r => New.Type SourceRegion Typed -> Eff r Core.Type
astTypeToCore (New.Type _ _ t') = case t' of
    New.TVar (Located _ tv) -> pure $ Core.TyVarTy $ TypeVariable tv TypeKind
    New.TFun t1 t2 -> Core.FuncTy <$> astTypeToCore t1 <*> astTypeToCore t2
    New.TUnit -> pure $ Core.ConTy unitCon
    New.TApp t1 t2 -> Core.AppTy <$> astTypeToCore t1 <*> astTypeToCore t2
    New.TUserDefined (Located _ qn) -> do
        let name = nameText <$> qn
        if
            | name == mkPrimQual (nameText intName) -> pure $ Core.ConTy intCon
            | name == mkPrimQual (nameText charName) -> pure $ Core.ConTy charCon
            | name == mkPrimQual (nameText stringName) -> pure $ Core.ConTy stringCon
            | name == mkPrimQual (nameText unitName) -> pure $ Core.ConTy unitCon
            | name == mkPrimQual (nameText ioName) -> pure $ Core.ConTy ioCon
            | otherwise -> do
                tyCon <- lookupTyCon name
                pure $ Core.ConTy tyCon
    New.TRecord _fields -> error "astTypeToCore: Record types not yet supported in Core"
    New.TList t1 -> do
        t1' <- astTypeToCore t1
        pure $ Core.AppTy (Core.ConTy listCon) t1'
      where
        -- TODO: proper list TyCon
        listCon = TyCon (mkPrimQual "List") (TyADT [])
    New.TExtension v -> absurd v

conToVar :: DataCon -> Core.Var
conToVar dc@(Core.DataCon n t _) = Core.Id (Global n) t (Just dc)

-- | Strip the Located wrapper from inside a VarRef, producing an UnlocatedVarRef
stripVarRefLoc :: forall n. VarRef n -> UnlocatedVarRef n
stripVarRefLoc (Global (Located _ qn)) = Global qn
stripVarRefLoc (Local (Located _ un)) = Local un

toCore :: HasCallStack => InnerToCoreC r => TypedExpr -> Eff r CoreExpr
toCore le@(New.Expr _ _ e) = moveTypeApplications <$> toCore' e
  where
    -- \| Move type applications to the left, eg '(f x) @Int' becomes 'f @Int x'
    moveTypeApplications :: CoreExpr -> CoreExpr
    moveTypeApplications (Core.TyApp (Core.App x y) t) = Core.App (Core.TyApp x t) y
    moveTypeApplications x = x

    toCore' :: InnerToCoreC r => TypedExpr' -> Eff r CoreExpr
    toCore' = \case
        New.EInt i -> pure $ Lit (Core.Int i)
        New.EFloat f -> pure $ Lit (Core.Double f)
        New.EString s -> pure $ Lit (Core.String s)
        New.EChar c -> pure $ Lit (Core.Char c)
        New.EUnit -> pure $ Lit Core.Unit
        New.EVar t (Located _ vr@(Global (Located _ _))) -> do
            t' <- eitherTypeToCore t
            let stripped = stripVarRefLoc vr
            pure $ Core.Var (Core.Id (nameText @VarName <$> stripped) t' Nothing)
        New.EVar t (Located _ v@(Local (Located _ _))) -> do
            t' <- eitherTypeToCore t
            let stripped = stripVarRefLoc v
            pure $ Core.Var (Core.Id (nameText @VarName <$> stripped) t' Nothing)
        New.ECon NoExtension v -> do
            ctor <- lookupCtor v
            pure $ Core.Var (conToVar ctor)
        New.ELam NoExtension (TypedLambdaParam vn t) body -> do
            t'' <- typeToCore t
            Core.Lam (Core.Id (Local (nameText <$> vn)) t'' Nothing) <$> toCore body
        New.EApp NoExtension e1 e2 -> do
            e1' <- toCore e1
            e2' <- toCore e2
            pure (App e1' e2')
        New.ETyApp e1 t1 -> do
            e1' <- toCore e1
            t1' <- astTypeToCore t1
            pure (Core.TyApp e1' t1')
        New.EIf cond ifTrue ifFalse -> do
            cond' <- toCore cond
            ifTrue' <- toCore ifTrue
            ifFalse' <- toCore ifFalse
            pure $
                Core.Match
                    cond'
                    Nothing
                    [ (Core.DataAlt trueCtor, [], ifTrue')
                    , (Core.DataAlt falseCtor, [], ifFalse')
                    ]
        New.EMatch e1 pats -> desugarMatch e1 pats
        New.ELet{} -> throwError (LetInTopLevel le)
        New.ELetIn NoExtension vn e1 e2 -> do
            e1' <- toCore e1
            e2' <- toCore e2
            let ref = Local (nameText <$> (vn ^. unlocated))
            let New.Expr _ e1Meta _ = e1
            t' <- typeToCore e1Meta
            debug $ "Let-binding" <+> pretty vn <+> ":" <+> pretty e1Meta <+> "=" <+> pretty e1'
            let bindingIsRecursive = isRecursive (vn ^. unlocated) e1
            debug $ "Recursive?" <+> pretty bindingIsRecursive
            pure $
                Core.Let
                    ( if bindingIsRecursive
                        then Recursive [(Core.Id ref t' Nothing, e1')]
                        else NonRecursive (Core.Id ref t' Nothing, e1')
                    )
                    e2'
        New.EBlock exprs -> desugarBlock exprs
        New.EAnn expr' _ty -> toCore expr'
        New.EExtension v -> absurd v

-- | Check if a variable is recursive in an expression
isRecursive :: Unique VarName -> TypedExpr -> Bool
isRecursive vn = go
  where
    go (New.Expr _ _ e') = case e' of
        New.EVar _ (Located _ (Local (Located _ n))) -> n == vn
        New.EVar _ _ -> False
        New.ELam _ _ body -> go body
        New.EApp _ e1 e2 -> go e1 || go e2
        New.ELetIn _ _ e1 e2 -> go e1 || go e2
        New.ELet _ _ e1 -> go e1
        New.EIf c t f -> go c || go t || go f
        New.EMatch e1 cases -> go e1 || any (\(_, rhs) -> go rhs) cases
        New.EBlock exprs -> any go exprs
        New.ETyApp e1 _ -> go e1
        New.EAnn e1 _ -> go e1
        New.ECon _ _ -> False
        New.EInt _ -> False
        New.EFloat _ -> False
        New.EString _ -> False
        New.EChar _ -> False
        New.EUnit -> False
        New.EExtension v -> absurd v

desugarMatch :: HasCallStack => InnerToCoreC r => TypedExpr -> [(TypedPattern, TypedExpr)] -> Eff r CoreExpr
desugarMatch e pats = do
    -- Scrutinee to Core and bind it to a fresh local, as Core.Match expects.
    e' <- toCore e
    logDebug $ "e': " <> pretty e'
    s0 <- mkBindName e

    -- Compile RHSs first; build a 1-column matrix from (pattern, rhsCore).
    branches <- for pats $ \(p, rhs) -> do
        rhs' <- toCore rhs
        pure (p, rhs')

    logDebug $ "Branches: " <> pretty (length branches) <> " branch(es)"

    let matrix = Match.buildMatrix1 branches

    -- Fresh locals used for constructor field binders within the matrix compiler.
    let freshLocal base ty = logDebugWith ("freshLocal: " <> pretty (base, ty)) $ do
            u <- makeUnique base
            pure (Core.Id (Local u) ty Nothing)

    compiled <- Match.compileMatrix lookupPrimCtor freshLocal [s0] matrix

    -- Bind the scrutinee to s0 before the compiled match to avoid a redundant DEFAULT match.
    pure $ Core.Let (NonRecursive (s0, e')) compiled

mkBindName :: InnerToCoreC r => TypedExpr -> Eff r Var
mkBindName (New.Expr _ _ (New.EVar varType (Located _ vn))) = do
    t' <- eitherTypeToCore varType
    unique <- makeUnique (nameText $ varRefVal vn)
    pure (Core.Id (Local unique) t' Nothing)
mkBindName (New.Expr _ t _) = do
    t' <- typeToCore t
    unique <- makeUnique "bind"
    pure (Core.Id (Local unique) t' Nothing)

desugarBlock :: InnerToCoreC r => NonEmpty TypedExpr -> Eff r CoreExpr
desugarBlock (a :| []) = toCore a
desugarBlock _ = error "todo"
