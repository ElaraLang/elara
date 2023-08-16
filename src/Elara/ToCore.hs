-- | Converts typed AST to Core
module Elara.ToCore where

import Control.Lens (to, view, (^.), _2)
import Data.Generics.Product
import Data.Generics.Wrapped
import Data.Map qualified as M
import Data.Traversable (for)
import Elara.AST.Generic as AST
import Elara.AST.Module (Module (Module))
import Elara.AST.Name (NameLike (..), Qualified (..), TypeName)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Select (LocatedAST (Typed))
import Elara.AST.StripLocation
import Elara.AST.Typed
import Elara.AST.VarRef (UnlocatedVarRef, VarRef' (Global, Local), varRefVal)
import Elara.Core as Core
import Elara.Core.Module (CoreDeclaration (..), CoreModule (..))
import Elara.Data.Pretty (Pretty (..))
import Elara.Data.Unique (Unique, UniqueGen, makeUnique)
import Elara.Error (ReportableError (..), writeReport)
import Elara.Prim (mkPrimQual)
import Elara.TypeInfer.Monotype qualified as Scalar
import Elara.TypeInfer.Type qualified as Type
import Error.Diagnose (Report (..))
import Polysemy (Member, Sem)
import Polysemy.Error
import Polysemy.State

import Elara.Data.Kind (ElaraKind (TypeKind))
import Elara.Prim.Core
import Polysemy.State.Extra (scoped)
import TODO (todo)

data ToCoreError
    = LetInTopLevel TypedExpr
    | UnknownConstructor (Located (Qualified TypeName))
    | UnknownPrimConstructor (Qualified Text)
    | UnknownLambdaType (Type.Type SourceRegion)

instance ReportableError ToCoreError where
    report (LetInTopLevel e) = writeReport $ Err (Just "LetInTopLevel") ("TODO") [] []
    report (UnknownConstructor (Located _ qn)) = writeReport $ Err (Just "UnknownConstructor") (pretty qn) [] []
    report (UnknownPrimConstructor qn) = writeReport $ Err (Just "UnknownPrimConstructor") (pretty qn) [] []
    report (UnknownLambdaType t) = writeReport $ Err (Just "UnknownLambdaType") (pretty t) [] []

type CtorSymbolTable = Map (Qualified Text) DataCon

type TyVarTable = Map Text TypeVariable

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

lookupTyVar :: HasCallStack => ToCoreC r => Text -> Sem r TypeVariable
lookupTyVar n = do
    table <- get @TyVarTable
    case M.lookup n table of
        Just v -> pure v
        Nothing -> error ("TODO: lookupTyVar " <> show n <> " " <> show table)

addTyVar :: ToCoreC r => Text -> TypeVariable -> Sem r ()
addTyVar n v = modify @TyVarTable (M.insert n v)

type ToCoreC r = (Member (Error ToCoreError) r, Member (State CtorSymbolTable) r, Member (State TyVarTable) r, Member UniqueGen r)

runToCoreC :: Sem (State TyVarTable : State CtorSymbolTable : Error ToCoreError : r) a -> Sem r (Either ToCoreError a)
runToCoreC =
    runError
        . evalState primCtorSymbolTable
        . evalState @TyVarTable mempty

moduleToCore :: HasCallStack => (ToCoreC r) => Module Typed -> Sem r CoreModule
moduleToCore (Module (Located _ m)) = do
    let name = m ^. field' @"name" . unlocated
    decls <- for (m ^. field' @"declarations") $ \decl -> do
        (body', var) <- case decl ^. _Unwrapped . unlocated . field' @"body" . _Unwrapped . unlocated of
            Value v _ _ -> do
                (ty, v') <- scoped @TyVarTable $ do
                    -- clear the tyvar table for each value
                    ty <- typeToCore (v ^. _Unwrapped . _2)
                    v' <- toCore v
                    pure (ty, v')
                let var = Core.Id (mkGlobalRef (nameText <$> (decl ^. _Unwrapped . unlocated . field' @"name" . unlocated))) ty
                pure (v', var)
        pure (CoreValue $ NonRecursive (var, body'))
    pure $ CoreModule name decls

typeToCore :: HasCallStack => (ToCoreC r) => Type.Type SourceRegion -> Sem r Core.Type
typeToCore (Type.Forall _ _ tv _ t) = do
    tv' <- makeUnique tv
    addTyVar tv (TypeVariable tv' TypeKind)
    typeToCore t
typeToCore (Type.VariableType{Type.name}) = do
    tv <- lookupTyVar name
    pure (Core.TyVarTy tv)
typeToCore (Type.Function{Type.input, Type.output}) = Core.FuncTy <$> typeToCore input <*> typeToCore output
typeToCore (Type.List _ t) = Core.AppTy listCon <$> typeToCore t
typeToCore (Type.Scalar _ Scalar.Text) = pure stringCon
typeToCore (Type.Scalar _ Scalar.Integer) = pure intCon
typeToCore (Type.Scalar _ Scalar.Unit) = pure unitCon
typeToCore (Type.Custom _ n args) = do
    args' <- traverse typeToCore args
    let con = Core.ConTy (mkPrimQual n)
    pure (foldl' Core.AppTy con args')
typeToCore other = error ("TODO: typeToCore " <> show other)

conToVar :: DataCon -> Core.Var
conToVar (Core.DataCon n t) = Core.Id (Global $ Identity n) t

mkLocalRef :: Unique n -> UnlocatedVarRef n
mkLocalRef = Local . Identity

mkGlobalRef :: Qualified n -> UnlocatedVarRef n
mkGlobalRef = Global . Identity

toCore :: HasCallStack => (ToCoreC r) => TypedExpr -> Sem r CoreExpr
toCore le@(Expr (Located _ e, t)) = toCore' e
  where
    toCore' :: (ToCoreC r) => TypedExpr' -> Sem r CoreExpr
    toCore' = \case
        AST.Int i -> pure $ Lit (Core.Int i)
        AST.Float f -> pure $ Lit (Core.Double f)
        AST.String s -> pure $ Lit (Core.String s)
        AST.Char c -> pure $ Lit (Core.Char c)
        AST.Unit -> pure $ Lit Core.Unit
        AST.Var (Located _ v) -> do
            t' <- typeToCore t
            pure $ Core.Var (Core.Id (nameText <$> stripLocation v) t')
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
        AST.FunctionCall e1 e2 -> Core.App <$> toCore e1 <*> toCore e2
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
            pure $
                Core.App
                    (Core.App (Core.Var $ Core.Id ref todo) x') -- x
                    xs'
        AST.Match e pats -> desugarMatch e pats
        AST.Let{} -> throw (LetInTopLevel le)
        AST.LetIn (Located _ vn) _ e1 e2 -> do
            e1' <- toCore e1
            e2' <- toCore e2
            let isRecursive = False -- todo
            let ref = mkLocalRef (nameText <$> vn)
            t' <- typeToCore t
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
            pure $ Core.App (Core.App (Core.Var (Core.Id ref todo)) one') two'
        AST.Tuple _ -> error "TODO: tuple with more than 2 elements"
        AST.Block exprs -> desugarBlock exprs

desugarMatch :: ToCoreC r => TypedExpr -> [(TypedPattern, TypedExpr)] -> Sem r CoreExpr
desugarMatch e pats = do
    e' <- toCore e
    bind' <- mkBindName e

    pats' <- for pats $ \(AST.Pattern (Located _ p, t), branch) -> do
        branch' <- toCore branch
        case p of
            AST.IntegerPattern i -> pure (Core.LitAlt $ Core.Int i, [], branch')
            AST.FloatPattern f -> pure (Core.LitAlt $ Core.Double f, [], branch')
            AST.StringPattern s -> pure (Core.LitAlt $ Core.String s, [], branch')
            AST.CharPattern c -> pure (Core.LitAlt $ Core.Char c, [], branch')
            AST.UnitPattern -> pure (Core.LitAlt Core.Unit, [], branch')
            AST.WildcardPattern -> pure (Core.DEFAULT, [], branch')
            AST.VarPattern (Located _ vn) -> do
                t' <- typeToCore t
                pure (Core.DEFAULT, [Core.Id (mkLocalRef (view (to nameText) <$> vn)) t'], branch')
            AST.ConstructorPattern cn pats -> do
                c <- lookupCtor cn
                pure (Core.DataAlt c, [], branch')
            -- todo: bind the pattern variables
            AST.ListPattern [] -> do
                t' <- typeToCore t
                pure (Core.DataAlt (Core.DataCon emptyListCtorName t'), [], branch')
            AST.ConsPattern (Pattern (Located _ p1, _)) (Pattern (Located _ p2, _)) -> do
                c <- lookupPrimCtor consCtorName
                pure (Core.DataAlt c, [], branch')
            other -> error $ "TODO: pattern "

    pure $ Core.Match e' (Just bind') pats'

mkBindName :: ToCoreC r => TypedExpr -> Sem r Var
mkBindName (AST.Expr (Located _ (AST.Var (Located _ vn)), t)) = do
    t' <- typeToCore t
    unique <- makeUnique (nameText $ varRefVal vn)

    pure (Core.Id (mkLocalRef unique) t')
mkBindName (AST.Expr (_, t)) = do
    t' <- typeToCore t
    unique <- makeUnique "bind"
    pure (Core.Id (mkLocalRef unique) t')

desugarBlock :: ToCoreC r => NonEmpty TypedExpr -> Sem r CoreExpr
desugarBlock (one :| []) = toCore one
desugarBlock _ = error "todo"
