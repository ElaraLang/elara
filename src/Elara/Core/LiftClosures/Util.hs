module Elara.Core.LiftClosures.Util where

import Data.Set qualified as Set
import Elara.AST.VarRef
import Elara.Core qualified as Core
import Elara.Core.ANF as ANF
import Elara.Core.Analysis

-- | Get the type from a Var, or Nothing for TyVar
varTermType :: Core.Var -> Maybe Core.Type
varTermType (Core.Id _ t _) = Just t
varTermType (Core.TyVar _) = Nothing

-- | Check if a variable is global
isGlobal :: Core.Var -> Bool
isGlobal (Core.Id (Global _) _ _) = True
isGlobal _ = False

-- | Check if a variable is local
isLocal :: Core.Var -> Bool
isLocal (Core.Id (Local _) _ _) = True
isLocal _ = False

-- | Filter to only local variables
filterLocals :: Set Core.Var -> Set Core.Var
filterLocals = Set.filter isLocal

-- | Compute captured variables (free locals minus the binding itself)
computeCaptures :: Core.Var -> Set Core.Var -> [Core.Var]
computeCaptures bound freeVars =
    Set.toList $ filterLocals $ Set.delete bound freeVars

-- | Check if an atom is a lambda
isLam :: ANF.AExpr b -> Bool
isLam (ANF.Lam _ _) = True
isLam _ = False

-- | Peel arguments from nested lambdas
peelLam :: CExpr Core.Var -> ([Core.Var], Expr Core.Var)
peelLam (ANF.AExpr (ANF.Lam v body)) =
    case body of
        ANF.CExpr nested ->
            let (vs, final) = peelLam nested
             in (v : vs, final)
        letExpr -> ([v], letExpr)
peelLam other = ([], ANF.CExpr other)

-- | Build a nested lambda from a list of arguments and a body
mkNestedLam :: [Core.Var] -> Expr Core.Var -> Expr Core.Var
mkNestedLam args body = foldr mkLam body args
  where
    mkLam v b = ANF.CExpr $ ANF.AExpr $ ANF.Lam v b

{- | Create the lifted type for a function given its captures and original type.
That is, if the original type is `a -> b` and the captures are of types `c1, c2`,
the lifted type will be `c1 -> c2 -> a -> b`.
-}
liftedType :: Traversable f => f Core.Var -> Core.Type -> Maybe Core.Type
liftedType captures originalType = do
    captureTypes <- traverse varTermType captures
    pure $ foldr Core.FuncTy originalType captureTypes

-- | Get the closed local variables in an expression
closedVariables :: FreeCoreVars ast => ast Core.Var -> Set Core.Var
closedVariables e = filerLocal $ freeCoreVars e
  where
    filerLocal =
        Set.filter
            ( \case
                Core.Id (Local _) _ _ -> True
                _ -> False
            )
