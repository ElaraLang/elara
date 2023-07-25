{-# LANGUAGE RecordWildCards #-}

-- | Converts typed AST to Core
module Elara.ToCore where

import Control.Lens (to, view, (^.))
import Data.Map qualified as M
import Data.Traversable (for)
import Elara.AST.Lenses (HasDeclarationBody' (unlocatedDeclarationBody'))
import Elara.AST.Module (Module (Module), module'Declarations, module'Name)
import Elara.AST.Name (ModuleName (ModuleName), NameLike (..), Qualified (..), TypeName)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Select (HasDeclarationName (declarationName), Typed)
import Elara.AST.StripLocation
import Elara.AST.Typed as AST
import Elara.AST.VarRef (VarRef' (Global, Local), varRefVal)
import Elara.Core as Core
import Elara.Core.Module (CoreDeclaration (CoreDeclaration), CoreDeclarationBody (CoreValue), CoreModule (..))
import Elara.Data.Pretty (Pretty (..))
import Elara.Data.Unique (UniqueGen, makeUnique)
import Elara.Error (ReportableError (..), writeReport)
import Elara.Prim (mkPrimQual)
import Elara.TypeInfer.Type qualified as Type
import Elara.TypeInfer.Monotype qualified as Scalar
import Error.Diagnose (Report (..))
import Polysemy (Member, Sem)
import Polysemy.Error
import Polysemy.State

import Elara.Data.Kind (ElaraKind (TypeKind))

data ToCoreError
    = LetInTopLevel AST.Expr
    | UnknownConstructor (Located (Qualified TypeName))
    | UnknownPrimConstructor (Qualified Text)
    | UnknownLambdaType (Type.Type SourceRegion)
    deriving (Show, Eq)

instance ReportableError ToCoreError where
    report (LetInTopLevel e) = writeReport $ Err (Just "LetInTopLevel") (pretty e) [] []
    report (UnknownConstructor (Located _ qn)) = writeReport $ Err (Just "UnknownConstructor") (pretty qn) [] []
    report (UnknownPrimConstructor qn) = writeReport $ Err (Just "UnknownPrimConstructor") (pretty qn) [] []
    report (UnknownLambdaType t) = writeReport $ Err (Just "UnknownLambdaType") (pretty t) [] []

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

type ToCoreC r = (Member (Error ToCoreError) r, Member (State CtorSymbolTable) r, Member UniqueGen r)

runToCoreC :: Sem (State CtorSymbolTable : Error ToCoreError : r) a -> Sem r (Either ToCoreError a)
runToCoreC = runError . evalState primCtorSymbolTable

moduleToCore :: (ToCoreC r) => Module Typed -> Sem r CoreModule
moduleToCore (Module (Located _ m)) = do
    let name = nameText (m ^. module'Name)
    decls <- for (m ^. module'Declarations) $ \decl -> do
        body' <- case decl ^. unlocatedDeclarationBody' of
            Value v -> CoreValue <$> toCore v
            other -> error "TODO: other declaration types"
        pure (CoreDeclaration (decl ^. declarationName . to fullNameText) body')
    pure $ CoreModule name decls

typeToCore :: (ToCoreC r) => Type.Type SourceRegion -> Sem r Core.Type
typeToCore (Type.VariableType{name}) = do
    name' <- makeUnique name
    pure (Core.TyVarTy (Core.TypeVariable name' TypeKind))
typeToCore (Type.Function{input, output}) = Core.FuncTy <$> typeToCore input <*> typeToCore output
typeToCore (Type.Forall _ _ _ _ t) = typeToCore t
typeToCore (Type.List _ t) = Core.AppTy listCon <$> typeToCore t
typeToCore (Type.Scalar _ Scalar.String) = pure (Core.ConTy (mkPrimQual "String"))
typeToCore other = error ("TODO: typeToCore " <> show other)

conToVar :: DataCon -> Core.Var
conToVar (Core.DataCon n t) = Core.Id (Global $ Identity n) t

mkLocalRef = Local . Identity

mkGlobalRef = Global . Identity

toCore :: (ToCoreC r) => AST.Expr -> Sem r CoreExpr
toCore le@(AST.Expr (Located _ e, t)) = toCore' e
  where
    toCore' :: (ToCoreC r) => AST.Expr' -> Sem r CoreExpr
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
            let t = conToVar ctor
            pure $ Core.Var t
        AST.Lambda (Located _ vn) body -> do
            -- figure out the type of vn from the type of the lambda
            let extractLambdaInput (Type.Function{input, ..}) = pure input
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
                    (Core.App (Core.Var $ Core.Id ref _) x') -- x
                    xs'
        AST.Match e pats -> desugarMatch e pats
        AST.Let{} -> throw (LetInTopLevel le)
        AST.LetIn (Located _ vn) e1 e2 -> do
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
            pure $ Core.App (Core.App (Core.Var (Core.Id ref _)) one') two'
        AST.Tuple _ -> error "TODO: tuple with more than 2 elements"
        AST.Block exprs -> desugarBlock exprs

desugarMatch :: ToCoreC r => AST.Expr -> [(AST.Pattern, AST.Expr)] -> Sem r CoreExpr
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
            other -> error $ "TODO: pattern " <> show other

    pure $ Core.Match e' (Just bind') pats'

mkBindName :: ToCoreC r => AST.Expr -> Sem r Var
mkBindName (AST.Expr (Located _ (AST.Var (Located _ vn)), t)) = do
    t' <- typeToCore t
    unique <- makeUnique (nameText $ varRefVal vn)

    pure (Core.Id (mkLocalRef unique) t')
mkBindName (AST.Expr (_, t)) = do
    t' <- typeToCore t
    unique <- makeUnique "bind"
    pure (Core.Id (mkLocalRef unique) t')

desugarBlock :: ToCoreC r => NonEmpty AST.Expr -> Sem r CoreExpr
desugarBlock (one :| []) = toCore one
desugarBlock _ = error "todo"

trueCtorName :: Qualified Text
trueCtorName = Qualified "True" (ModuleName ("Elara" :| ["Prim"]))

falseCtorName :: Qualified Text
falseCtorName = Qualified "False" (ModuleName ("Elara" :| ["Prim"]))

emptyListCtorName :: Qualified Text
emptyListCtorName = Qualified "[]" (ModuleName ("Elara" :| ["Prim"]))

consCtorName :: Qualified Text
consCtorName = Qualified "::" (ModuleName ("Elara" :| ["Prim"]))

tuple2CtorName :: Qualified Text
tuple2CtorName = Qualified "Tuple2" (ModuleName ("Elara" :| ["Prim"]))

boolCon = ConTy (mkPrimQual "Bool")

listCon = ConTy (mkPrimQual "[]")

trueCtor = DataCon trueCtorName boolCon

falseCtor = DataCon falseCtorName boolCon

tuple2Con = ConTy (mkPrimQual "Tuple2")

tuple2Ctor = DataCon tuple2CtorName tuple2Con
