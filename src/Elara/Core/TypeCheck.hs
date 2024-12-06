{- | Type *checking* for the Core language.
This functions as a sanity check to make sure that any optimisations haven't broken the structure
of the program to form an invalid program

It doesn't do any inference! As Core is already typed, it just checks that the types are consistent
-}
module Elara.Core.TypeCheck where

import Elara.Core.ANF qualified as ANF

import Data.Set qualified as Set

import Elara.AST.VarRef
import Elara.Core (CoreExpr, Var (..))
import Elara.Core qualified as Core
import Elara.Core.Analysis (freeCoreVars, freeTypeVars)
import Elara.Core.Generic
import Elara.Core.Module
import Elara.Core.ToANF (fromANF, fromANFAtom)
import Elara.Data.Pretty
import Elara.Error
import Elara.Logging (StructuredDebug, debug, debugWith)
import Elara.Prim.Core
import Elara.TypeInfer.Type (Polytype (Forall), functionMonotypeResult)
import Polysemy
import Polysemy.Error
import Polysemy.State (State, evalState, get, modify)
import Polysemy.State.Extra (locally, scoped)
import TODO (todo)

data TypeCheckError
    = UnknownVariable Var (Set.Set (UnlocatedVarRef Text))
    | CoreTypeMismatch
        { expected :: Core.Type
        , actual :: Core.Type
        , source :: (CoreExpr, CoreExpr)
        }
    | CoreTypeMismatchIncompleteExpected
        { incompleteExpected :: Text
        , actual :: Core.Type
        , source :: (CoreExpr, CoreExpr)
        }
    | UnificationError CoreExpr CoreExpr
    | InfiniteType Var CoreExpr
    | OccursCheck Var CoreExpr
    deriving (Show, Eq, Generic)

instance Pretty TypeCheckError

instance ReportableError TypeCheckError

data TcState = TcState
    { scope :: Set.Set (UnlocatedVarRef Text)
    -- ^ The 'Var' already holds the variable's type so we don't need to track that.
    -- However we do need to track scoping, as an optimisation could pull a variable out of scope
    }

addToScope :: Var -> TcState -> TcState
addToScope (Id name _ _) s = s{scope = Set.insert name (scope s)}
addToScope _ s = s

isInScope :: Var -> TcState -> Bool
isInScope (Id name@(Local _) _ _) s = Set.member name (scope s)
isInScope (Id (Global _) _ _) _ = True -- Global vars are always in scope
isInScope _ _ = False

typeCheckCoreModule :: (Member (Error TypeCheckError) r, Member StructuredDebug r) => CoreModule (Bind Var ANF.Expr) -> Sem r ()
typeCheckCoreModule (CoreModule n m) = do
    let initialState = TcState{scope = mempty}

    _ <- evalState initialState $ do
        for_ m $ \case
            CoreValue (NonRecursive (v, e)) -> scoped $ do
                modify (addToScope v)
                eType <- typeCheck e
                pure ()
            CoreValue (Recursive bs) -> scoped $ do
                for_ bs $ \(v, e) -> do
                    modify (addToScope v)

                for_ bs $ \(v, e) -> typeCheck e
            CoreType _ -> pure ()

    pure ()

varType :: Var -> Core.Type
varType (TyVar _) = error "TyVar"
varType (Id _ t _) = t

typeCheck :: (Member (Error TypeCheckError) r, Member (State TcState) r, Member StructuredDebug r) => ANF.Expr Var -> Sem r (Core.Type)
typeCheck (ANF.Let bind in') = do
    case bind of
        NonRecursive (v, e) -> do
            eType <- typeCheckC e
            locally (addToScope v) $
                typeCheck in'
        Recursive binds -> scoped $ do
            let vars = map fst binds
            for_ vars $ \v -> modify (addToScope v)
            for_ binds $ \(_, e) -> typeCheckC e
            typeCheck in'
typeCheck (ANF.CExpr cExp) = typeCheckC cExp

typeCheckC :: (Member (Error TypeCheckError) r, Member (State TcState) r, Member StructuredDebug r) => ANF.CExpr Var -> Sem r Core.Type
typeCheckC (ANF.App f x) = debugWith ("App " <> pretty (fromANFAtom f) <+> pretty (fromANFAtom x)) $ do
    fType <- typeCheckA f
    debug $ "fType: " <> pretty fType
    xType <- typeCheckA x
    debug $ "xType: " <> pretty xType
    case fType of
        Core.ForAllTy tv t -> do
            -- this is probably wrong lol
            let t' = Core.substTypeVar tv xType t
            debug $ "t': " <> pretty t'
            retType <- typeCheckC (ANF.App (ANF.TyApp f xType) x)
            pure retType
        Core.FuncTy argType retType -> do
            if argType == xType
                then pure retType
                else throw $ CoreTypeMismatch argType xType ((fromANFAtom f), (fromANFAtom x))
        other -> throw $ CoreTypeMismatchIncompleteExpected (prettyToText $ pretty xType <+> "-> something" ) other ((fromANFAtom f), (fromANFAtom x))
typeCheckC (ANF.AExpr aExp) = typeCheckA aExp
typeCheckC (ANF.Match e of' alts) = scoped $ do
    eType <- typeCheckA e
    whenJust of' $ \v -> modify (addToScope v)
    altTypes <- for alts $ \(con, bs, e) -> do
        for_ bs $ \v -> modify (addToScope v)
        case con of
            Core.DEFAULT -> do
                eType' <- typeCheck e
                pure eType'
            Core.LitAlt lit -> do
                let litType = typeCheckLit lit
                eType' <- typeCheck e
                if litType == eType
                    then pure eType'
                    else throw $ CoreTypeMismatch litType eType ((fromANF e), (fromANF e))
            Core.DataAlt con' -> do
                let conType = Core.functionTypeResult $ con'.dataConType
                debug $ "conType: " <> pretty conType <+> (parens $ pretty $ generalize con'.dataConType)
                eType' <- typeCheck e
                -- TODO more robust type checking here with the binders and stuff
                debug $ "eType': " <> pretty eType'
                if generalize eType `equalUnderSubst` generalize conType
                    then pure eType'
                    else
                        throw $
                            CoreTypeMismatch
                                (conType)
                                eType
                                ((fromANF e), (fromANF e))

    case altTypes of
        [] -> error "empty match? how do we handle this"
        x : _ -> pure $ x

typeCheckLit :: Core.Literal -> Core.Type
typeCheckLit lit = case lit of
    Core.Int _ -> Core.ConTy intCon
    Core.String _ -> Core.ConTy stringCon
    Core.Char _ -> Core.ConTy charCon
    Core.Double _ -> Core.ConTy doubleCon
    Core.Unit -> Core.ConTy unitCon

typeCheckA ::
    (Member (Error TypeCheckError) r, Member (State TcState) r, Member StructuredDebug r) =>
    ANF.AExpr Var -> Sem r Core.Type
typeCheckA (ANF.Lit lit) = pure $ typeCheckLit lit
-- Globally qualified vars are always in scope
typeCheckA (ANF.Var v) = do
    env <- get
    case isInScope v env of
        True -> pure (varType v)
        False -> throw $ UnknownVariable v (env.scope)
typeCheckA (ANF.Lam v body) = do
    let t = varType v

    eType <- locally (addToScope v) $ typeCheck body
    pure $ Core.FuncTy t eType
typeCheckA (ANF.TyApp e t) = do
    eType <- typeCheckA e
    case eType of
        Core.ForAllTy tv t' -> pure $ Core.substTypeVar tv t t'
        t' ->
            throw $
                CoreTypeMismatchIncompleteExpected
                    { incompleteExpected = "A polymorphic type"
                    , actual = t'
                    , source = (fromANFAtom e, fromANFAtom (ANF.TyApp e t))
                    }
typeCheckA (ANF.TyLam t e) = do
    todo

{- | Relation that defines 2 types as equal iff they are equal under a substitution of type variables
For example @forall a. a@ and @forall b. b@ are equal in this relation,
but @forall a b. a -> b@ and @forall a b. b -> a@ are not equal
-}
equalUnderSubst :: Core.Type -> Core.Type -> Bool
equalUnderSubst (Core.ForAllTy tv1 t1) (Core.ForAllTy tv2 t2) =
    equalUnderSubst t1 (Core.substTypeVar tv2 (Core.TyVarTy tv1) t2)
equalUnderSubst (Core.FuncTy a1 b1) (Core.FuncTy a2 b2) =
    equalUnderSubst a1 a2 && equalUnderSubst b1 b2
equalUnderSubst (Core.AppTy a1 b1) (Core.AppTy a2 b2) =
    equalUnderSubst a1 a2 && equalUnderSubst b1 b2
equalUnderSubst (Core.ConTy c1) (Core.ConTy c2) = c1 == c2
equalUnderSubst (Core.TyVarTy tv1) (Core.TyVarTy tv2) = tv1 == tv2
equalUnderSubst _ _ = False

generalize :: Core.Type -> Core.Type
generalize t = let ftv = freeTypeVars t in foldr Core.ForAllTy t ftv
