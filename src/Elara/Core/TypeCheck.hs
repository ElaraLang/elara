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
import Elara.Core.Generic
import Elara.Core.Module
import Elara.Core.ToANF (fromANF, fromANFAtom)
import Elara.Data.Pretty
import Elara.Error
import Elara.Prim.Core
import Polysemy
import Polysemy.Error
import Polysemy.State (State, evalState, get, modify)
import Polysemy.State.Extra (locally, scoped)
import TODO (todo)

data TypeCheckError
    = UnboundVariable Var (Set.Set (UnlocatedVarRef Text))
    | TypeMismatch
        { expected :: Core.Type
        , actual :: Core.Type
        , source :: (CoreExpr, CoreExpr)
        }
    | TypeMismatchIncompleteExpected
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

typeCheckCoreModule :: Member (Error TypeCheckError) r => CoreModule (Bind Var ANF.Expr) -> Sem r ()
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

typeCheck :: (Member (Error TypeCheckError) r, Member (State TcState) r) => ANF.Expr Var -> Sem r (Core.Type)
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

typeCheckC :: (Member (Error TypeCheckError) r, Member (State TcState) r) => ANF.CExpr Var -> Sem r Core.Type
typeCheckC (ANF.App f x) = do
    fType <- typeCheckA f
    xType <- typeCheckA x
    case fType of
        Core.FuncTy argType retType -> do
            if argType == xType
                then pure retType
                else throw $ TypeMismatch argType xType ((fromANFAtom f), (fromANFAtom x))
        other -> throw $ TypeMismatch (Core.FuncTy fType xType) other ((fromANFAtom f), (fromANFAtom x))
typeCheckC (ANF.AExpr aExp) = typeCheckA aExp
typeCheckC (ANF.Match e of' alts) = scoped $ do
    eType <- typeCheckA e
    whenJust of' $ \v -> modify (addToScope v)
    altTypes <- for alts $ \(con, bs, e) -> do
        case con of
            Core.DEFAULT -> do
                eType' <- typeCheck e
                pure eType'
            Core.LitAlt lit -> do
                let litType = typeCheckLit lit
                eType' <- typeCheck e
                if litType == eType
                    then pure eType'
                    else throw $ TypeMismatch litType eType ((fromANF e), (fromANF e))
            Core.DataAlt con' -> do
                let conType = Core.ConTy (con'.dataConDataType)
                eType' <- typeCheck e
                -- TODO more robust type checking here with the binders and stuff
                if conType == eType
                    then pure eType'
                    else throw $ TypeMismatch conType eType ((fromANF e), (fromANF e))

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

typeCheckA :: (Member (Error TypeCheckError) r, Member (State TcState) r) => ANF.AExpr Var -> Sem r Core.Type
typeCheckA (ANF.Lit lit) = pure $ typeCheckLit lit
-- Globally qualified vars are always in scope
typeCheckA (ANF.Var v) = do
    env <- get

    case isInScope v env of
        True -> pure (varType v)
        False -> throw $ UnboundVariable v (env.scope)
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
                TypeMismatchIncompleteExpected
                    { incompleteExpected = "A polymorphic type"
                    , actual = t'
                    , source = (fromANFAtom e, fromANFAtom (ANF.TyApp e t))
                    }
typeCheckA (ANF.TyLam t e) = do
    todo
