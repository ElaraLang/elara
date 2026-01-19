{-# LANGUAGE ImpredicativeTypes #-}

{- | Type *checking* for the Core language.
This functions as a sanity check to make sure that any optimisations haven't broken the structure
of the program to form an invalid program

It doesn't do any inference! As Core is already typed, it just checks that the types are consistent
-}
module Elara.Core.TypeCheck (typeCheckCoreModule, TypeCheckError (..)) where

import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static
import Effectful.State.Extra (locally, scoped)
import Effectful.State.Static.Local (State, evalState, get, modify)
import Elara.AST.VarRef
import Elara.Core (CoreExpr, Var (..))
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Analysis (freeTypeVars)
import Elara.Core.Generic
import Elara.Core.Module
import Elara.Core.ToANF (fromANF, fromANFAtom, fromANFCExpr)
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Logging (StructuredDebug, logDebug)
import Elara.Prim.Core
import TODO (todo)

data TypeCheckError
    = UnknownVariable {unknownVariable :: Var, unknownVariableScope :: Set.Set (UnlocatedVarRef Text)}
    | CoreTypeMismatch
        { expected :: Core.Type
        , actual :: Core.Type
        , source :: (CoreExpr, CoreExpr)
        -- ^ (the expression that had the expected type, and the expression that had the actual type)
        }
    | CoreTypeMismatchIncompleteExpected
        { incompleteExpected :: Text
        , actual :: Core.Type
        , source :: (CoreExpr, CoreExpr)
        }
    | UnificationError CoreExpr CoreExpr
    | InfiniteType Var CoreExpr
    | OccursCheck Var CoreExpr
    | PatternMatchMissingBinders {alt :: Core.AltCon, altType :: Core.Type, providedBinders :: [Var], expr :: CoreExpr}
    deriving (Show, Generic)

instance Pretty TypeCheckError

instance ReportableError TypeCheckError where
    errorCode = \case
        UnknownVariable{} -> Just Codes.unknownVariableTC
        CoreTypeMismatch{} -> Just Codes.coreTypeMismatch
        _ -> Nothing @Codes.ErrorCode

newtype TcState = TcState
    { scope :: Set.Set (UnlocatedVarRef Text)
    {- ^ The 'Var' already holds the variable's type so we don't need to track that.
    However we do need to track scoping, as an optimisation could pull a variable out of scope
    -}
    }

addToScope :: Var -> TcState -> TcState
addToScope (Id name _ _) s = s{scope = Set.insert name (scope s)}
addToScope _ s = s

isInScope :: Var -> TcState -> Bool
isInScope (Id name@(Local _) _ _) s = Set.member name (scope s)
isInScope (Id (Global _) _ _) _ = True -- Global vars are always in scope
isInScope _ _ = False

typeCheckCoreModule :: (Error TypeCheckError :> r, HasCallStack, StructuredDebug :> r) => CoreModule (Bind Var ANF.Expr) -> Eff r ()
typeCheckCoreModule (CoreModule _moduleName m) = do
    let initialState = TcState{scope = mempty}

    _ <- evalState initialState $ for_ m $ \case
        CoreValue (NonRecursive (v, e)) -> scoped $ do
            modify (addToScope v)
            typeCheck e
            pass
        CoreValue (Recursive bs) -> scoped $ do
            for_ bs $ \(v, _e) -> do
                modify (addToScope v)

            for_ bs $ \(_v, e) -> typeCheck e
        CoreType _ -> pass

    pass

varType :: Var -> Core.Type
varType (TyVar _) = error "TyVar"
varType (Id _ t _) = t

typeCheck :: (Error TypeCheckError :> r, State TcState :> r, StructuredDebug :> r, HasCallStack) => ANF.Expr Var -> Eff r Core.Type
typeCheck (ANF.Let bind in') = case bind of
    NonRecursive (v, e) -> do
        typeCheckC e
        locally (addToScope v) $
            typeCheck in'
    Recursive binds -> scoped $ do
        let vars = map fst binds
        for_ vars $ \v -> modify (addToScope v)
        for_ binds $ \(_, e) -> typeCheckC e
        typeCheck in'
typeCheck (ANF.CExpr cExp) = typeCheckC cExp

typeCheckC ::
    ( HasCallStack
    , Error TypeCheckError :> r
    , State TcState :> r
    , StructuredDebug :> r
    , HasCallStack
    ) =>
    ANF.CExpr Var -> Eff r Core.Type
typeCheckC (ANF.App f x) = do
    fType <- typeCheckA f
    logDebug $ "fType: " <> pretty fType
    xType <- typeCheckA x
    logDebug $ "xType: " <> pretty xType
    case fType of
        Core.FuncTy argType retType -> do
            if generalize argType `equalUnderSubst` generalize xType
                then pure retType
                else throwError $ CoreTypeMismatch argType xType (fromANFAtom f, fromANFAtom x)
        other -> throwError $ CoreTypeMismatchIncompleteExpected (prettyToText $ pretty xType <+> "-> something") other (fromANFAtom f, fromANFAtom x)
typeCheckC (ANF.AExpr aExp) = typeCheckA aExp
typeCheckC match@(ANF.Match scrutinee of' alts) = scoped $ do
    scrutineeType <- typeCheckA scrutinee -- the type of the scrutinee
    logDebug $ "scrutineeType: " <> pretty scrutineeType
    whenJust of' $ \v -> modify (addToScope v)
    altTypes <- for alts $ \(con, bs, altExpr) -> do
        for_ bs $ \v -> modify (addToScope v)
        case con of
            Core.DEFAULT -> do
                typeCheck altExpr
            Core.LitAlt lit -> do
                let litType = typeCheckLit lit
                altExprType <- typeCheck altExpr
                if litType == scrutineeType
                    then pure altExprType
                    else throwError $ CoreTypeMismatch litType scrutineeType (fromANFCExpr match, fromANF altExpr)
            Core.DataAlt con' -> do
                let conType = Core.functionTypeResult con'.dataConType
                logDebug $ "conType: " <> pretty conType <+> parens (pretty $ generalize con'.dataConType)
                when (length bs /= length (Core.functionTypeArgs con'.dataConType)) $ do
                    -- debug $ "bs: " <> pretty bs
                    -- debug $ "functionTypeArgs: " <> pretty (Core.functionTypeArgs con'.dataConType)
                    throwError $
                        PatternMatchMissingBinders con con'.dataConType bs (fromANFCExpr match)
                altExprType <- typeCheck altExpr
                -- TODO more robust type checking here with the binders and stuff
                logDebug $ "altExprType: " <> pretty altExprType
                let generalizedScrutineeType = generalize scrutineeType
                let generalizedConType = generalize conType
                logDebug $ "generalized:" <+> pretty generalizedScrutineeType <+> "and" <+> pretty generalizedConType
                logDebug $ "equal?" <+> pretty (generalizedScrutineeType `equalUnderSubst` generalizedConType)
                -- the type of the scrutinee must match the type of the constructor
                if generalizedScrutineeType `equalUnderSubst` generalizedConType
                    then pure altExprType
                    else
                        throwError $
                            CoreTypeMismatch
                                generalizedConType
                                generalizedScrutineeType
                                (fromANFAtom scrutinee, fromANF altExpr)

    case altTypes of
        [] -> error "empty match? how do we handle this"
        x : _ -> pure x

typeCheckLit :: Core.Literal -> Core.Type
typeCheckLit lit = case lit of
    Core.Int _ -> Core.ConTy intCon
    Core.String _ -> Core.ConTy stringCon
    Core.Char _ -> Core.ConTy charCon
    Core.Double _ -> Core.ConTy doubleCon
    Core.Unit -> Core.ConTy unitCon

typeCheckA ::
    (Error TypeCheckError :> r, State TcState :> r, StructuredDebug :> r, HasCallStack) =>
    ANF.AExpr Var -> Eff r Core.Type
typeCheckA (ANF.Lit lit) = pure $ typeCheckLit lit
-- Globally qualified vars are always in scope
typeCheckA (ANF.Var v) = do
    env <- get
    ( if isInScope v env
            then
                pure (varType v)
            else
                throwError $ UnknownVariable v env.scope
        )
typeCheckA (ANF.Lam v body) = do
    let t = varType v

    eType <- locally (addToScope v) $ typeCheck body
    pure $ Core.FuncTy t eType
typeCheckA (ANF.TyApp e t) = do
    eType <- typeCheckA e
    case eType of
        Core.ForAllTy tv t' -> pure $ Core.substTypeVar tv t t'
        t' ->
            throwError $
                CoreTypeMismatchIncompleteExpected
                    { incompleteExpected = "A polymorphic type"
                    , actual = t'
                    , source = (fromANFAtom e, fromANFAtom (ANF.TyApp e t))
                    }
typeCheckA (ANF.TyLam t e) = todo

{- | Relation that defines 2 types as equal iff they are equal under a substitution of type variables
For example @forall a. a@ and @forall b. b@ are equal in this relation,
but @forall a b. a -> b@ and @forall a b. b -> a@ are not equal
-}
equalUnderSubst :: Core.Type -> Core.Type -> Bool
equalUnderSubst x y =
    equalUnderSubst' x y || equalUnderSubst' y x -- reflexive

equalUnderSubst' :: Core.Type -> Core.Type -> Bool
equalUnderSubst' x y | x == y = True
equalUnderSubst' x y | x == unalias y = True
-- forall a. T and forall b. U are equal if T/[a=b] == U
equalUnderSubst' (Core.ForAllTy tv1 t1) (Core.ForAllTy tv2 t2) =
    equalUnderSubst t1 (Core.substTypeVar tv2 (Core.TyVarTy tv1) t2)
-- eg forall a. List a and List Int should be equal
equalUnderSubst' (Core.ForAllTy tv1 t1) t2 =
    equalUnderSubst (Core.substTypeVar tv1 (Core.TyVarTy tv1) t1) t2
equalUnderSubst' (Core.FuncTy a1 b1) (Core.FuncTy a2 b2) =
    equalUnderSubst a1 a2 && equalUnderSubst b1 b2
equalUnderSubst' (Core.AppTy a1 b1) (Core.AppTy a2 b2) =
    equalUnderSubst a1 a2 && equalUnderSubst b1 b2
equalUnderSubst' (Core.ConTy c1) (Core.ConTy c2) = c1 == c2
-- At this point (after substituting foralls), a type variable should match anything
equalUnderSubst' (Core.TyVarTy _) _ = True
equalUnderSubst' x y = False

unalias :: Core.Type -> Core.Type
unalias (Core.ConTy (Core.TyCon _ (Core.TyAlias underlying))) = unalias underlying
unalias (Core.AppTy f x) = Core.AppTy (unalias f) (unalias x)
unalias (Core.FuncTy a b) = Core.FuncTy (unalias a) (unalias b)
unalias (Core.ForAllTy tv t) = Core.ForAllTy tv (unalias t)
unalias other = other

generalize :: Core.Type -> Core.Type
generalize t = let ftv = freeTypeVars t in foldr Core.ForAllTy t ftv
