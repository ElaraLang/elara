module Elara.Core.Analysis where

import Elara.Core (CoreExpr, Expr (..), TyCon, Var (..), typeArity)
import Elara.Core qualified as Core

import Data.List (maximum)
import Data.Set qualified as Set
import Elara.Core.ANF qualified as ANF
import Elara.Core.Generic (Bind (..), binders)
import Elara.Data.Pretty
import Elara.Prim.Core (charCon, doubleCon, intCon, stringCon, unitCon)
import Print (showPretty)

estimateArity :: CoreExpr -> Int
estimateArity (Var (TyVar _)) = error "Type variable in expression"
estimateArity (Var (Id _ t _)) = typeArity t
estimateArity (Lit _) = 0
estimateArity (App f _) = estimateArity f - 1
estimateArity (TyApp f _) = estimateArity f
estimateArity (Lam _ e) = 1 + estimateArity e
estimateArity (TyLam _ e) = estimateArity e
estimateArity (Let _ e) = estimateArity e
estimateArity (Match _ _ alts) = maximum (map (\(_, _, e) -> estimateArity e) alts)

declaredLambdaArity :: CoreExpr -> Int
declaredLambdaArity (Lam _ e) = 1 + declaredLambdaArity e
declaredLambdaArity _ = 0

findTyCon :: Core.Type -> Maybe TyCon
findTyCon (Core.ConTy tc) = Just tc
findTyCon (Core.ForAllTy _ t) = findTyCon t
findTyCon (Core.AppTy t _) = findTyCon t
findTyCon _ = Nothing

exprType :: (HasCallStack, Pretty Core.Type, Pretty (Expr Var)) => CoreExpr -> Core.Type
exprType (Var v) = varType v
exprType (Lit l) = literalType l
exprType app@(App f _) = case exprType f of
    Core.FuncTy _ t -> t
    Core.ForAllTy _ t -> t
    t -> error $ "exprType: expected function type, got " <> showPretty t <> " in " <> showPretty app
exprType (TyApp f t) = case exprType f of
    Core.ForAllTy tv t' -> Core.substTypeVar tv t t'
    t' -> error $ "exprType: expected forall type, got " <> show t' <> " in " <> show (TyApp f t)
exprType (Lam b e) = Core.FuncTy (varType b) (exprType e)
exprType (TyLam _ e) = exprType e
exprType (Let _ e) = exprType e
exprType (Match _ _ alts) = case alts of
    [] -> error "exprType: empty match"
    (_, _, e) : _ -> exprType e

varType :: Var -> Core.Type
varType (TyVar tv) = Core.TyVarTy tv
varType (Id _ t _) = t

literalType :: Core.Literal -> Core.Type
literalType (Core.Int _) = Core.ConTy intCon
literalType (Core.String _) = Core.ConTy stringCon
literalType (Core.Char _) = Core.ConTy charCon
literalType (Core.Double _) = Core.ConTy doubleCon
literalType Core.Unit = Core.ConTy unitCon

class FreeCoreVars ast where
    freeCoreVars :: Ord b => ast b -> Set b

instance FreeCoreVars Core.Expr where
    freeCoreVars (Core.Var v) = one v
    freeCoreVars (Core.Lit _) = Set.empty
    freeCoreVars (Core.App f x) = freeCoreVars f <> freeCoreVars x
    freeCoreVars (Core.TyApp f _) = freeCoreVars f
    freeCoreVars (Core.Lam b e) = Set.delete b (freeCoreVars e)
    freeCoreVars (Core.TyLam _ e) = freeCoreVars e
    freeCoreVars (Core.Let bind body) = do
        let bound = binders bind
        freeCoreVars body `Set.difference` Set.fromList bound
    freeCoreVars (Core.Match e _ alts) = freeCoreVars e <> foldMap (\(_, bs, e) -> freeCoreVars e `Set.difference` Set.fromList bs) alts

instance FreeCoreVars ANF.AExpr where
    freeCoreVars (ANF.Var v) = one v
    freeCoreVars (ANF.Lit _) = Set.empty
    freeCoreVars (ANF.TyApp f _) = freeCoreVars f
    freeCoreVars (ANF.Lam b e) = Set.delete b (freeCoreVars e)
    freeCoreVars (ANF.TyLam _ e) = freeCoreVars e

instance FreeCoreVars ANF.CExpr where
    freeCoreVars (ANF.App f x) = freeCoreVars f <> freeCoreVars x
    freeCoreVars (ANF.AExpr e) = freeCoreVars e
    freeCoreVars (ANF.Match e _ alts) = freeCoreVars e <> foldMap (\(_, bs, e) -> freeCoreVars e `Set.difference` Set.fromList bs) alts

instance FreeCoreVars ANF.Expr where
    freeCoreVars (ANF.Let bind body) = freeCoreVarsBind bind `Set.difference` freeCoreVars body
    freeCoreVars (ANF.CExpr e) = freeCoreVars e

freeCoreVarsBind :: (FreeCoreVars ast, Ord a) => Bind a ast -> Set a
freeCoreVarsBind (NonRecursive (_, e)) = freeCoreVars e
freeCoreVarsBind (Recursive bs) = foldMap (freeCoreVars . snd) bs

freeTypeVars :: Core.Type -> Set Core.TypeVariable
freeTypeVars (Core.TyVarTy tv) = one tv
freeTypeVars (Core.ConTy _) = Set.empty
freeTypeVars (Core.FuncTy a b) = freeTypeVars a <> freeTypeVars b
freeTypeVars (Core.ForAllTy tv t) = Set.delete tv (freeTypeVars t)
freeTypeVars (Core.AppTy a b) = freeTypeVars a <> freeTypeVars b
