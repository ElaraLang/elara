module Elara.Core.Analysis where

import Elara.Core (CoreExpr, Expr (..), TyCon, Var (..), typeArity)
import Elara.Core qualified as Core

import Data.List (maximum)

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
findTyCon (Core.FuncTy _ t) = findTyCon t
findTyCon (Core.AppTy t _) = findTyCon t
findTyCon _ = Nothing
