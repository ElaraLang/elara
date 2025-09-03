{-# LANGUAGE PatternSynonyms #-}

-- | Converts typed AST to Core
module Elara.ToCore where

import Data.Generics.Product
import Data.Generics.Sum (AsAny (_As))
import Data.Generics.Wrapped
import Data.Map qualified as M
import Elara.AST.Generic as AST
import Elara.AST.Generic.Common (NoFieldValue (..))
import Elara.AST.Module (Module (Module))
import Elara.AST.Name (Name (..), NameLike (..), Qualified (..), TypeName, VarName)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Select (LocatedAST (Typed))
import Elara.AST.StripLocation
import Elara.AST.Typed
import Elara.AST.VarRef (VarRef' (Global, Local), varRefVal, pattern UnlocatedGlobal, pattern UnlocatedLocal)
import Elara.Core as Core
import Elara.Core.Generic (Bind (..))
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..), CoreTypeDecl (CoreTypeDecl), CoreTypeDeclBody (CoreDataDecl))
import Elara.Core.Pretty ()
import Elara.Data.Kind (ElaraKind (..))
import Elara.Data.Pretty (Pretty (..), vcat, (<+>))
import Elara.Data.TopologicalGraph
import Elara.Data.Unique.Effect
import Elara.Error (ReportableError (..), runErrorOrReport, writeReport)
import Elara.Logging
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.Prim (mkPrimQual)
import Elara.Prim.Core
import Elara.ToCore.Match qualified as Match
import Elara.TypeInfer.Type qualified as Type
import Elara.Utils (uncurry3)
import Error.Diagnose (Report (..))
import Optics
import Polysemy (Members, Sem, subsume)
import Polysemy.Error
import Polysemy.State
import TODO (todo)

data ToCoreError
    = LetInTopLevel !TypedExpr
    | UnknownConstructor !(Located (Qualified TypeName)) CtorSymbolTable
    | UnknownPrimConstructor !(Qualified Text)
    | UnknownTypeConstructor !(Qualified Text) CtorSymbolTable
    | UnknownLambdaType !(Type.Type SourceRegion)
    | HasCallStack => UnsolvedTypeSnuckIn !(Type.Type SourceRegion)
    | UnknownVariable !(Located (Qualified Name))

instance ReportableError ToCoreError where
    report (LetInTopLevel _) = writeReport $ Err (Just "LetInTopLevel") "TODO" [] []
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
                    , vcat $ pretty <$> M.keys (syms.tyCons)
                    ]
                )
                []
                []
    report (UnknownLambdaType t) = writeReport $ Err (Just "UnknownLambdaType") todo [] []
    report (UnsolvedTypeSnuckIn t) = do
        writeReport $
            Err
                (Just "UnsolvedTypeSnuckIn")
                (vcat [todo, pretty $ prettyCallStack callStack])
                [ todo
                ]
                []
    report (UnknownVariable (Located _ qn)) = writeReport $ Err (Just "UnknownVariable") (pretty qn) [] []

data CtorSymbolTable = CtorSymbolTable
    { dataCons :: Map (Qualified Text) DataCon
    , tyCons :: Map (Qualified Text) TyCon
    }

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

lookupCtor :: ToCoreC r => Located (Qualified TypeName) -> Sem r DataCon
lookupCtor qn = do
    table <- get @CtorSymbolTable
    let plainName = nameText <$> qn ^. unlocated
    case M.lookup plainName table.dataCons of
        Just ctor -> pure ctor
        Nothing -> throw (UnknownConstructor qn table)

registerCtor :: ToCoreC r => DataCon -> Sem r ()
registerCtor ctor = modify (\s -> s{dataCons = M.insert (ctor ^. field @"name") ctor s.dataCons})

registerTyCon :: ToCoreC r => TyCon -> Sem r ()
registerTyCon ctor@(TyCon name _) = modify (\s -> s{tyCons = M.insert name ctor s.tyCons})

lookupPrimCtor :: ToCoreC r => Qualified Text -> Sem r DataCon
lookupPrimCtor qn = do
    table <- get @CtorSymbolTable
    case M.lookup qn (table.dataCons) of
        Just ctor -> pure ctor
        Nothing -> throw (UnknownPrimConstructor qn)

lookupTyCon :: ToCoreC r => Qualified Text -> Sem r TyCon
lookupTyCon qn = do
    table <- get @CtorSymbolTable
    case M.lookup qn table.tyCons of
        Just ctor -> pure ctor
        Nothing -> throw (UnknownTypeConstructor qn table)

type VariableTable = Map (Qualified Name) (Type.Type SourceRegion)

type ToCoreEffects = [State CtorSymbolTable, Error ToCoreError, UniqueGen, StructuredDebug]

type InnerToCoreEffects = [State CtorSymbolTable, Error ToCoreError, UniqueGen, StructuredDebug]

type ToCoreC r = (Members ToCoreEffects r)

type InnerToCoreC r = (Members InnerToCoreEffects r)

-- runToCorePipeline :: IsPipeline r => Sem (EffectsAsPrefixOf ToCoreEffects r) a -> Sem r a
-- runToCorePipeline =
--     subsume
--         . uniqueGenToIO
--         . runErrorOrReport
--         . evalState primCtorSymbolTable

-- moduleToCore :: HasCallStack => ToCoreC r => Module 'Typed -> Sem r (CoreModule CoreBind)
-- moduleToCore (Module (Located _ m)) = debugWith ("Converting module: " <> pretty (m ^. field' @"name")) $ do
--     let name = m ^. field' @"name" % unlocated
--     let declGraph = createGraph (m ^. field' @"declarations")
--     decls <- for (allEntriesRevTopologically declGraph) $ \decl -> do
--         case decl ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
--             Value n v _ _ _ -> debugWith ("Value decl: " <+> pretty n) $ do
--                 ty <- typeToCore (v ^. _Unwrapped % _2)
--                 v' <- toCore v
--                 let var = Core.Id (UnlocatedGlobal (nameText <$> n ^. unlocated)) ty Nothing
--                 let rec = isRecursive (n ^. unlocated) v (_1 % _As @"Global" % unlocated)
--                 pure $ Just $ CoreValue $ if rec then Recursive [(var, v')] else NonRecursive (var, v')
--             TypeDeclaration n tvs (Located _ (ADT ctors)) (TypeDeclAnnotations _ kind) -> debugWith ("Type decl: " <+> pretty n) $ do
--                 let cleanedTypeDeclName = nameText <$> (n ^. unlocated)
--                 let tyCon =
--                         TyCon
--                             cleanedTypeDeclName
--                             ( TyADT
--                                 (ctors ^.. each % _1 % unlocated % to (fmap nameText))
--                             )
--                 debug (pretty tyCon)
--                 registerTyCon tyCon
--                 ctors' <- for ctors $ \(Located _ n, ctorArgs) -> do
--                     ctorArgs' <- traverse (typeToCore . fst) ctorArgs
--                     let ctorType =
--                             foldr
--                                 (Core.ForAllTy . mkTypeVar . view unlocated)
--                                 ( foldr
--                                     Core.FuncTy
--                                     (flipfoldl' (flip Core.AppTy . TyVarTy . mkTypeVar . view unlocated) (ConTy tyCon) tvs)
--                                     ctorArgs'
--                                 )
--                                 tvs
--                     debug $ "Ctor: " <+> pretty n <+> " with type: " <+> pretty ctorType
--                     pure (nameText <$> n, ctorType, tyCon)
--                 let ctors'' = fmap (uncurry3 DataCon) ctors'
--                 traverse_ registerCtor ctors''
--                 pure $
--                     Just $
--                         CoreType $
--                             CoreTypeDecl
--                                 cleanedTypeDeclName
--                                 kind
--                                 (tvs ^.. each % unlocated % to mkTypeVar)
--                                 (CoreDataDecl (toList ctors''))
--             TypeDeclaration n tvs (Located _ (Alias (t, _))) (TypeDeclAnnotations _ kind) -> do
--                 todo
--     -- t' <- typeToCore t
--     -- let ty = foldr (Core.ForAllTy . typedTvToCoreTv) t' tvs
--     -- pure $ Just $ CoreType $ CoreTypeDecl declName kind (fmap typedTvToCoreTv tvs) (CoreTypeAlias ty)
--     pure $ CoreModule name (catMaybes decls)

-- mkTypeVar :: Select "TypeVar" 'Typed -> Core.TypeVariable
-- mkTypeVar tv = TypeVariable tv TypeKind

-- polytypeToCore :: HasCallStack => InnerToCoreC r => Type.Polytype SourceRegion -> Sem r Core.Type
-- polytypeToCore (Type.Forall tvs constraints t) = do
--     t' <- typeToCore t
--     let tvs' = fmap mkTypeVar tvs
--     pure $ foldr Core.ForAllTy t' tvs'

-- eitherTypeToCore :: HasCallStack => InnerToCoreC r => Type.Type SourceRegion -> Sem r Core.Type
-- eitherTypeToCore (Type.Polytype p) = polytypeToCore p
-- eitherTypeToCore (Type.Lifted t) = typeToCore t

-- typeToCore :: HasCallStack => InnerToCoreC r => Type.Monotype SourceRegion -> Sem r Core.Type
-- typeToCore (Type.TypeVar (Type.SkolemVar v)) = pure $ Core.TyVarTy $ TypeVariable v TypeKind
-- typeToCore (Type.TypeVar (Type.UnificationVar v)) = pure $ Core.TyVarTy $ TypeVariable v TypeKind
-- typeToCore (Type.Scalar Type.ScalarInt) = pure $ Core.ConTy intCon
-- typeToCore (Type.Scalar Type.ScalarFloat) = pure $ Core.ConTy floatCon
-- typeToCore (Type.Scalar Type.ScalarString) = pure $ Core.ConTy stringCon
-- typeToCore (Type.Scalar Type.ScalarChar) = pure $ Core.ConTy charCon
-- typeToCore (Type.Scalar Type.ScalarUnit) = pure $ Core.ConTy unitCon
-- typeToCore (Type.Function t1 t2) = Core.FuncTy <$> typeToCore t1 <*> typeToCore t2
-- typeToCore (Type.TypeConstructor qn ts) = debugWith ("Type constructor: " <+> pretty qn <+> " with args: " <+> pretty ts) $ do
--     tyCon <- lookupTyCon (fmap (view _Unwrapped) qn)
--     ts' <- traverse typeToCore ts
--     pure $ foldl' Core.AppTy (Core.ConTy tyCon) ts'

-- conToVar :: DataCon -> Core.Var
-- conToVar dc@(Core.DataCon n t _) = Core.Id (Global $ Identity n) t (Just dc)

-- toCore :: HasCallStack => InnerToCoreC r => TypedExpr -> Sem r CoreExpr
-- toCore le@(Expr (Located _ e, t)) = moveTypeApplications <$> toCore' e
--   where
--     -- \| Move type applications to the left, eg '(f x) @Int' becomes 'f @Int x'
--     moveTypeApplications :: CoreExpr -> CoreExpr
--     moveTypeApplications (Core.TyApp (Core.App x y) t) = Core.App (Core.TyApp x t) y
--     moveTypeApplications x = x

--     toCore' :: InnerToCoreC r => TypedExpr' -> Sem r CoreExpr
--     toCore' = \case
--         AST.Int i -> pure $ Lit (Core.Int i)
--         AST.Float f -> pure $ Lit (Core.Double f)
--         AST.String s -> pure $ Lit (Core.String s)
--         AST.Char c -> pure $ Lit (Core.Char c)
--         AST.Unit -> pure $ Lit Core.Unit
--         AST.Var (Located _ (vr@(Global _), t)) -> do
--             t' <- eitherTypeToCore t

--             pure $ Core.Var (Core.Id (nameText @VarName <$> stripLocation vr) t' Nothing)
--         AST.Var (Located _ (v@(Local _), t)) -> do
--             t' <- eitherTypeToCore t
--             pure $ Core.Var (Core.Id (nameText @VarName <$> stripLocation v) t' Nothing)
--         AST.Constructor v -> do
--             ctor <- lookupCtor v
--             pure $ Core.Var (conToVar ctor)
--         AST.Lambda (Located _ (TypedLambdaParam (vn, t))) body -> do
--             t'' <- typeToCore t

--             Core.Lam (Core.Id (UnlocatedLocal (nameText <$> vn)) t'' Nothing) <$> toCore body
--         AST.FunctionCall e1 e2 -> do
--             e1' <- toCore e1
--             e2' <- toCore e2
--             pure (App e1' e2')
--         AST.TypeApplication e1 t1 -> do
--             e1' <- toCore e1
--             t1' <- typeToCore t1
--             pure (Core.TyApp e1' t1')
--         AST.If cond ifTrue ifFalse -> do
--             cond' <- toCore cond
--             ifTrue' <- toCore ifTrue
--             ifFalse' <- toCore ifFalse
--             pure $
--                 Core.Match
--                     cond'
--                     Nothing
--                     [ (Core.DataAlt trueCtor, [], ifTrue')
--                     , (Core.DataAlt falseCtor, [], ifFalse')
--                     ]
--         AST.Match e pats -> desugarMatch e pats
--         AST.Let{} -> throw (LetInTopLevel le)
--         AST.LetIn (Located _ vn) NoFieldValue e1 e2 -> do
--             e1' <- toCore e1
--             e2' <- toCore e2
--             -- TODO: we need to detect mutually recursive bindings
--             let ref = UnlocatedLocal (nameText <$> vn)
--             t' <- typeToCore (typeOf e1)
--             debug $ "Let-binding" <+> pretty vn <+> ":" <+> pretty (typeOf e1) <+> "=" <+> pretty e1'
--             let bindingIsRecursive = isRecursive vn e1 (_1 % _As @"Local" % unlocated)
--             debug $ "Recursive?" <+> pretty bindingIsRecursive
--             pure $
--                 Core.Let
--                     ( if bindingIsRecursive
--                         then Recursive [(Core.Id ref t' Nothing, e1')]
--                         else NonRecursive (Core.Id ref t' Nothing, e1')
--                     )
--                     e2'
--         AST.Block exprs -> desugarBlock exprs

-- {- | Check if a variable is recursive in an expression
-- Takes the name of the variable, the expression, and a lens to the variable name (Local or Global usually)
-- -}
-- isRecursive vn e1 l =
--     anyOf
--         (cosmosOf gplate % _Unwrapped % _1 % unlocated % _Ctor' @"Var" % unlocated % l)
--         (== vn)
--         e1

-- desugarMatch :: HasCallStack => InnerToCoreC r => TypedExpr -> [(TypedPattern, TypedExpr)] -> Sem r CoreExpr
-- desugarMatch e pats = do
--     -- Scrutinee to Core and bind it to a fresh local, as Core.Match expects.
--     e' <- toCore e
--     s0 <- mkBindName e

--     -- Compile RHSs first; build a 1-column matrix from (pattern, rhsCore).
--     branches <- for pats $ \(p, rhs) -> do
--         rhs' <- toCore rhs
--         pure (p, rhs')

--     let matrix = Match.buildMatrix1 branches

--     -- Resolve constructors by qualified text from the current constructor table.
--     let resolveByText qn = do
--             table <- get @CtorSymbolTable
--             case M.lookup qn (table.dataCons) of
--                 Just ctor -> pure ctor
--                 Nothing -> throw (UnknownPrimConstructor qn)

--     -- Fresh locals used for constructor field binders within the matrix compiler.
--     let freshLocal base ty = do
--             u <- makeUnique base
--             pure (Core.Id (UnlocatedLocal u) ty Nothing)

--     compiled <- Match.compileMatrix resolveByText freshLocal [s0] matrix

--     -- Bind the scrutinee to s0 before the compiled match to avoid a redundant DEFAULT match.
--     pure $ Core.Let (NonRecursive (s0, e')) compiled

-- mkBindName :: InnerToCoreC r => TypedExpr -> Sem r Var
-- mkBindName (AST.Expr (Located _ (AST.Var (Located _ (vn, varType))), t)) = do
--     t' <- eitherTypeToCore varType
--     unique <- makeUnique (nameText $ varRefVal vn)
--     pure (Core.Id (UnlocatedLocal unique) t' Nothing)
-- mkBindName (AST.Expr (_, t)) = do
--     t' <- typeToCore t
--     unique <- makeUnique "bind"
--     pure (Core.Id (UnlocatedLocal unique) t' Nothing)

-- desugarBlock :: InnerToCoreC r => NonEmpty TypedExpr -> Sem r CoreExpr
-- desugarBlock (a :| []) = toCore a
-- desugarBlock _ = error "todo"
