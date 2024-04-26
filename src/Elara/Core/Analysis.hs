module Elara.Core.Analysis where

import Elara.Core (CoreExpr, Expr (..), Var (..), typeArity)

import Data.List (maximum)

estimateArity :: CoreExpr -> Int
estimateArity (Var (TyVar _)) = error "Type variable in expression"
estimateArity (Var (Id _ t)) = typeArity t
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
