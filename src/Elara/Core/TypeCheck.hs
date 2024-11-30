{- | Type *checking* for the Core language.
This functions as a sanity check to make sure that any optimisations haven't broken the structure
of the program to form an invalid program

It doesn't do any inference! As Core is already typed, it just checks that the types are consistent
-}
module Elara.Core.TypeCheck where

import Elara.Core.ANF qualified as ANF

import Data.Set qualified as Set

import Elara.Core (CoreExpr, Expr (..), TyCon, Var (..), typeArity)
import Elara.Core qualified as Core
import Polysemy
import Polysemy.Error
import Elara.Core.Generic
import TODO (todo)
import Elara.Prim.Core
import Polysemy.State (gets, evalState, modify, State)
import qualified Data.Map as Map
import Polysemy.State.Extra (locally)
import Elara.Core.ToANF (fromANFCExpr, fromANFAtom, fromANF)
import Elara.Core.Module
import Elara.Error
import Elara.Data.Pretty

data TypeCheckError
    = UnboundVariable Var
    | TypeMismatch CoreExpr CoreExpr
    | UnificationError CoreExpr CoreExpr
    | InfiniteType Var CoreExpr
    | OccursCheck Var CoreExpr
    deriving (Show, Eq, Generic)

instance Pretty TypeCheckError 

instance ReportableError TypeCheckError

data TcState = TcState
    { env :: Map Var Core.Type
    }



typeCheckCoreModule :: Member (Error TypeCheckError) r => CoreModule (Bind Var ANF.Expr) -> Sem r ()
typeCheckCoreModule (CoreModule n m) = do
    -- let vType (Id _ t _) = [t]
    --     vType (TyVar _) = []
    -- let env = Map.fromList $ flip concatMap m $ \case
    --         CoreValue (NonRecursive (v, e)) -> [(v, fromANF e)]
    --         CoreValue (Recursive bs) -> map (\(v, e) -> (v, fromANF e)) bs
    --         CoreType _ -> []

    let initialState = TcState {env = mempty}
    
    _ <- evalState initialState $ do
        for_ m $ \case
            CoreValue (NonRecursive (v, e)) -> do
                eType <- typeCheck e
                modify (\s -> s {env = Map.insert v eType (env s)})
            CoreValue (Recursive bs) -> do
                for_ bs $ \(v, e) -> do
                    eType <- typeCheck e
                    modify (\s -> s {env = Map.insert v eType (env s)})
            CoreType _ -> pure ()
    
    pure ()
    


typeCheck :: (Member (Error TypeCheckError) r, Member (State TcState) r) => ANF.Expr Var -> Sem r (Core.Type)
typeCheck (ANF.Let _ _ ) = do
    todo
typeCheck (ANF.CExpr cExp) = typeCheckC cExp

typeCheckC :: (Member (Error TypeCheckError) r, Member (State TcState) r) => ANF.CExpr Var -> Sem r Core.Type
typeCheckC (ANF.App f x) = do
    fType <- typeCheckA f
    xType <- typeCheckA x
    case fType of
        Core.FuncTy argType retType -> do
            if argType == xType
                then if retType == xType
                    then pure retType
                    else throw $ TypeMismatch (fromANFAtom f) (fromANFAtom x)
                else throw $ TypeMismatch (fromANFAtom f) (fromANFAtom x)
        _ -> throw $ TypeMismatch (fromANFAtom f) (fromANFAtom x)
typeCheckC (ANF.AExpr aExp) = typeCheckA aExp
typeCheckC (ANF.Match _ _ alts) = do
    todo


typeCheckA :: (Member (Error TypeCheckError) r, Member (State TcState) r) => ANF.AExpr Var -> Sem r Core.Type
typeCheckA (ANF.Lit lit) = case lit of
    Core.Int _ -> pure $ Core.ConTy intCon
    Core.String _ -> pure $ Core.ConTy stringCon
    Core.Char _ -> pure $ Core.ConTy charCon
    Core.Double _ -> pure $ Core.ConTy doubleCon
    Core.Unit -> pure $ Core.ConTy unitCon
typeCheckA (ANF.Var v) = do
    env <- gets env
    case Map.lookup v env of
        Just t -> pure t
        Nothing -> throw $ UnboundVariable v
typeCheckA (ANF.Lam v e) = do
    case v of
        TyVar _ -> error "typeCheckA: TyVar"
        Id _ t _ -> do
            eType <- locally (\s -> s {env = Map.insert v t (env s)}) $ typeCheck e
            pure $ Core.FuncTy t eType
typeCheckA (ANF.TyApp e t) = do
    eType <- typeCheckA e
    case eType of
        Core.ForAllTy tv t' -> pure $ Core.substTypeVar tv t t'
        t' -> throw $ TypeMismatch (fromANFAtom e) (fromANFAtom e) -- TODO: better error message
typeCheckA (ANF.TyLam t e) = do
    todo
