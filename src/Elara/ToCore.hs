-- | Converts typed AST to Core
module Elara.ToCore where

import Data.Generics.Product
import Data.Generics.Wrapped
import Data.Map qualified as M
import Data.Traversable (for)
import Elara.AST.Generic as AST
import Elara.AST.Generic.Common
import Elara.AST.Module (Module (Module))
import Elara.AST.Name (Name (..), NameLike (..), Qualified (..), TypeName, VarName)
import Elara.AST.Region (Located (Located), SourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.AST.Select (LocatedAST (Typed))
import Elara.AST.StripLocation
import Elara.AST.Typed
import Elara.AST.VarRef (UnlocatedVarRef, VarRef' (Global, Local), varRefVal)
import Elara.Core as Core
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Data.Pretty (Pretty (..))
import Elara.Data.Unique (Unique, UniqueGen, makeUnique, uniqueGenToIO)
import Elara.Error (ReportableError (..), runErrorOrReport, writeReport)
import Elara.Pipeline (EffectsAsPrefixOf, IsPipeline)
import Elara.Prim (mkPrimQual)
import Elara.Prim.Core
import Elara.TypeInfer.Monotype qualified as Scalar
import Elara.TypeInfer.Type qualified as Type
import Error.Diagnose (Report (..))
import Error.Diagnose.Report (Marker (..))
import Polysemy (Members, Sem)
import Polysemy.Error
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State
import TODO (todo)

data ToCoreError
    = LetInTopLevel !TypedExpr
    | UnknownConstructor !(Located (Qualified TypeName))
    | UnknownPrimConstructor !(Qualified Text)
    | UnknownLambdaType !(Type.Type SourceRegion)
    | UnsolvedTypeSnuckIn !(Type.Type SourceRegion)
    | UnknownVariable !(Located (Qualified Name))

instance ReportableError ToCoreError where
    report (LetInTopLevel _) = writeReport $ Err (Just "LetInTopLevel") "TODO" [] []
    report (UnknownConstructor (Located _ qn)) = writeReport $ Err (Just "UnknownConstructor") (pretty qn) [] []
    report (UnknownPrimConstructor qn) = writeReport $ Err (Just "UnknownPrimConstructor") (pretty qn) [] []
    report (UnknownLambdaType t) = writeReport $ Err (Just "UnknownLambdaType") (pretty t) [] []
    report (UnsolvedTypeSnuckIn t) = do
        writeReport $
            Err
                (Just "UnsolvedTypeSnuckIn")
                (pretty t)
                [ (sourceRegionToDiagnosePosition t.location, Where "Type declared / created here")
                ]
                []
    report (UnknownVariable (Located _ qn)) = writeReport $ Err (Just "UnknownVariable") (pretty qn) [] []

type CtorSymbolTable = Map (Qualified Text) DataCon

primCtorSymbolTable :: CtorSymbolTable
primCtorSymbolTable =
    fromList
        [ (trueCtorName, trueCtor)
        , (falseCtorName, falseCtor)
        , (consCtorName, DataCon consCtorName listCon)
        , (emptyListCtorName, DataCon emptyListCtorName listCon)
        , (tuple2CtorName, tuple2Ctor)
        ]

lookupCtor :: ToCoreC r => Located (Qualified TypeName) -> Sem r DataCon
lookupCtor qn = do
    table <- get @CtorSymbolTable
    let plainName = nameText <$> qn ^. unlocated
    case M.lookup plainName table of
        Just ctor -> pure ctor
        Nothing -> throw (UnknownConstructor qn)

lookupPrimCtor :: ToCoreC r => Qualified Text -> Sem r DataCon
lookupPrimCtor qn = do
    table <- get @CtorSymbolTable
    case M.lookup qn table of
        Just ctor -> pure ctor
        Nothing -> throw (UnknownPrimConstructor qn)

type VariableTable = Map (Qualified Name) (Type.Type SourceRegion)

type ToCoreEffects = [State CtorSymbolTable, Error ToCoreError, UniqueGen]

type InnerToCoreEffects = [State CtorSymbolTable, Reader VariableTable, Error ToCoreError, UniqueGen]

type ToCoreC r = (Members ToCoreEffects r)
type InnerToCoreC r = (Members InnerToCoreEffects r)

runToCorePipeline :: IsPipeline r => Sem (EffectsAsPrefixOf ToCoreEffects r) a -> Sem r a
runToCorePipeline =
    uniqueGenToIO
        . runErrorOrReport
        . evalState primCtorSymbolTable

moduleToCore :: HasCallStack => VariableTable -> ToCoreC r => Module 'Typed -> Sem r CoreModule
moduleToCore vt (Module (Located _ m)) = runReader vt $ do
    let name = m ^. field' @"name" % unlocated
    decls <- for (m ^. field' @"declarations") $ \decl -> do
        bv <- case decl ^. _Unwrapped % unlocated % field' @"body" % _Unwrapped % unlocated of
            Value v _ _ _ -> do
                ty <- typeToCore (v ^. _Unwrapped % _2)
                v' <- toCore v
                let var = Core.Id (mkGlobalRef (nameText <$> decl ^. _Unwrapped % unlocated % field' @"name" % unlocated)) ty
                pure $ Just (v', var)
            TypeDeclaration{} -> pure Nothing
        pure ((\(a, b) -> CoreValue $ NonRecursive (b, a)) <$> bv)
    pure $ CoreModule name (catMaybes decls)

typeToCore :: HasCallStack => InnerToCoreC r => Type.Type SourceRegion -> Sem r Core.Type
typeToCore (Type.Forall _ _ tv _ t) = do
    t' <- typeToCore t
    pure (Core.ForAllTy (TypeVariable tv TypeKind) t')
typeToCore (Type.VariableType{Type.name}) = pure (Core.TyVarTy (TypeVariable name TypeKind))
typeToCore (Type.Function{Type.input, Type.output}) = Core.FuncTy <$> typeToCore input <*> typeToCore output
typeToCore (Type.List _ t) = Core.AppTy listCon <$> typeToCore t
typeToCore (Type.Scalar _ Scalar.Text) = pure stringCon
typeToCore (Type.Scalar _ Scalar.Integer) = pure intCon
typeToCore (Type.Scalar _ Scalar.Unit) = pure unitCon
typeToCore (Type.Scalar _ Scalar.Char) = pure charCon
typeToCore (Type.Scalar _ Scalar.Bool) = pure boolCon
typeToCore (Type.Custom _ n args) = do
    args' <- traverse typeToCore args
    let con = Core.ConTy (mkPrimQual n)
    pure (foldl' Core.AppTy con args')
typeToCore (Type.Tuple _ (a :| [b])) = do
    a' <- typeToCore a
    b' <- typeToCore b
    pure $ Core.AppTy (Core.AppTy tuple2Con a') b'
typeToCore (Type.Tuple _ x) = error $ "unsupported tuple length: " <> show (length x)
typeToCore unsolved@(Type.UnsolvedType{}) = throw (UnsolvedTypeSnuckIn unsolved)

conToVar :: DataCon -> Core.Var
conToVar (Core.DataCon n t) = Core.Id (Global $ Identity n) t

mkLocalRef :: Unique n -> UnlocatedVarRef n
mkLocalRef = Local . Identity

mkGlobalRef :: Qualified n -> UnlocatedVarRef n
mkGlobalRef = Global . Identity

toCore :: HasCallStack => InnerToCoreC r => TypedExpr -> Sem r CoreExpr
toCore le@(Expr (Located _ e, t)) = moveTypeApplications <$> toCore' e
  where
    -- \| Move type applications to the left, eg '(f x) @Int' becomes 'f @Int x'
    moveTypeApplications :: CoreExpr -> CoreExpr
    moveTypeApplications (Core.TyApp (Core.App x y) t) = Core.App (Core.TyApp x t) y
    moveTypeApplications x = x

    toCore' :: InnerToCoreC r => TypedExpr' -> Sem r CoreExpr
    toCore' = \case
        AST.Int i -> pure $ Lit (Core.Int i)
        AST.Float f -> pure $ Lit (Core.Double f)
        AST.String s -> pure $ Lit (Core.String s)
        AST.Char c -> pure $ Lit (Core.Char c)
        AST.Unit -> pure $ Lit Core.Unit
        AST.Var (Located _ vr@((Global l@(Located _ v)))) -> do
            vt <- ask @VariableTable
            t <- case M.lookup (NVarName <$> v) vt of
                Just t -> typeToCore t
                Nothing -> throw (UnknownVariable (NVarName <<$>> l))

            pure $ Core.Var (Core.Id (nameText @VarName <$> stripLocation vr) t)
        AST.Var (Located _ v@(Local _)) -> do
            t' <- typeToCore t
            pure $ Core.Var (Core.Id (nameText @VarName <$> stripLocation v) t')
        AST.Constructor v -> do
            ctor <- lookupCtor v
            pure $ Core.Var (conToVar ctor)
        AST.Lambda (Located _ vn) body -> do
            -- figure out the type of vn from the type of the lambda
            let extractLambdaInput (Type.Function{Type.input}) = pure input
                extractLambdaInput (Type.Forall _ _ _ _ t) = extractLambdaInput t
                extractLambdaInput other = throw (UnknownLambdaType other)
            t' <- extractLambdaInput t

            t'' <- typeToCore t'

            Core.Lam (Core.Id (mkLocalRef (nameText <$> vn)) t'') <$> toCore body
        AST.FunctionCall e1 e2 -> do
            e1' <- toCore e1
            e2' <- toCore e2
            pure (App e1' e2')
        AST.TypeApplication e1 t1 -> do
            e1' <- toCore e1
            t1' <- typeToCore t1
            pure (Core.TyApp e1' t1')
        AST.If cond ifTrue ifFalse -> do
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
        AST.List [] -> do
            t' <- typeToCore t
            let ref = mkGlobalRef emptyListCtorName
            pure $ Core.Var (Core.Id ref t')
        AST.List (x : xs) -> do
            x' <- toCore x
            xs' <- toCore' (AST.List xs)
            let ref = mkGlobalRef consCtorName
            consType' <- consType
            pure $
                Core.App
                    (Core.App (Core.Var $ Core.Id ref consType') x') -- x
                    xs'
        AST.Match e pats -> desugarMatch e pats
        AST.Let{} -> throw (LetInTopLevel le)
        AST.LetIn (Located _ vn) NoFieldValue e1 e2 -> do
            e1' <- toCore e1
            e2' <- toCore e2
            let isRecursive = False -- todo
            let ref = mkLocalRef (nameText <$> vn)
            t' <- typeToCore (typeOf e1)
            pure $
                Core.Let
                    ( if isRecursive then Core.Recursive [(Core.Id ref t', e1')] else Core.NonRecursive (Core.Id ref t', e1')
                    )
                    e2'
        AST.Tuple (one :| []) -> toCore one
        AST.Tuple (one :| [two]) -> do
            one' <- toCore one
            two' <- toCore two
            let ref = mkGlobalRef tuple2CtorName
            t <- tuple2CtorType
            pure $ Core.App (Core.App (Core.Var (Core.Id ref t)) one') two'
        AST.Tuple _ -> error "TODO: tuple with more than 2 elements"
        AST.Block exprs -> desugarBlock exprs

stripForAll :: Core.Type -> Core.Type
stripForAll (Core.ForAllTy _ t) = stripForAll t
stripForAll t = t

desugarMatch :: HasCallStack => InnerToCoreC r => TypedExpr -> [(TypedPattern, TypedExpr)] -> Sem r CoreExpr
desugarMatch e pats = do
    e' <- toCore e
    bind' <- mkBindName e

    pats' <- for pats $ \(p, branch) -> do
        (con, vars) <- patternToCore p
        branch' <- toCore branch
        pure (con, vars, branch')

    pure $ Core.Match e' (Just bind') pats'
  where
    patternToCore :: HasCallStack => InnerToCoreC r => TypedPattern -> Sem r (Core.AltCon, [Core.Var])
    patternToCore (Pattern (Located _ p, t)) = do
        t' <- typeToCore t
        case p of
            AST.IntegerPattern i -> pure (Core.LitAlt $ Core.Int i, [])
            AST.FloatPattern f -> pure (Core.LitAlt $ Core.Double f, [])
            AST.StringPattern s -> pure (Core.LitAlt $ Core.String s, [])
            AST.CharPattern c -> pure (Core.LitAlt $ Core.Char c, [])
            AST.UnitPattern -> pure (Core.LitAlt Core.Unit, [])
            AST.WildcardPattern -> pure (Core.DEFAULT, [])
            AST.VarPattern (Located _ vn) -> pure (Core.DEFAULT, [Core.Id (mkLocalRef (view (to nameText) <$> vn)) t'])
            AST.ConstructorPattern cn pats -> do
                c <- lookupCtor cn
                pats' <- for pats patternToCore
                pure (Core.DataAlt c, snd =<< pats')
            AST.ListPattern [] -> do
                t' <- typeToCore t
                pure (Core.DataAlt $ DataCon emptyListCtorName (AppTy listCon t'), [])
            AST.ListPattern (x : xs) -> do
                (_, vars) <- patternToCore x
                vars' <- (snd =<<) <$> traverse patternToCore xs
                pure (Core.DataAlt $ DataCon consCtorName listCon, vars <> vars')
            AST.ConsPattern x xs -> do
                (_, vars) <- patternToCore x
                vars' <- snd <$> patternToCore xs
                pure (Core.DataAlt $ DataCon consCtorName listCon, vars <> vars')

mkBindName :: InnerToCoreC r => TypedExpr -> Sem r Var
mkBindName (AST.Expr (Located _ (AST.Var (Located _ vn)), t)) = do
    t' <- typeToCore t
    unique <- makeUnique (nameText $ varRefVal vn)
    pure (Core.Id (mkLocalRef unique) t')
mkBindName (AST.Expr (_, t)) = do
    t' <- typeToCore t
    unique <- makeUnique "bind"
    pure (Core.Id (mkLocalRef unique) t')

desugarBlock :: InnerToCoreC r => NonEmpty TypedExpr -> Sem r CoreExpr
desugarBlock (a :| []) = toCore a
desugarBlock _ = error "todo"
