module Elara.Core.Analysis where

import Elara.Core (CoreExpr, Expr (..), TyCon, Var (..), typeArity)
import Elara.Core qualified as Core
import Elara.Core.Pretty ()

import Data.List (maximum)
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

exprType :: HasCallStack => CoreExpr -> Core.Type
exprType (Var v) = varType v
exprType (Lit l) = literalType l
exprType app@(App f _) = case exprType f of
    Core.FuncTy _ t -> t
    t -> error $ "exprType: expected function type, got " <> showPretty t <> " in " <> showPretty app
exprType (TyApp f t) = case exprType f of
    Core.ForAllTy tv t' -> Core.substTypeVar tv t t'
    t' -> error $ "exprType: expected forall type, got " <> showPretty t' <> " in " <> showPretty (TyApp f t)
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
