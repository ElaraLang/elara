module Elara.TypeInfer.ConstraintSolving where

import Data.Foldable1
import Elara.AST.Name
import Elara.AST.VarRef
import Elara.TypeInfer.Context (Context, Entry)
import Elara.TypeInfer.Type qualified
import Prelude hiding (Constraint)

type Type = Elara.TypeInfer.Type.Type ()

data Constraint
    = -- | Empty constraint (𝜖) used as a base case
      EmptyConstraint
    | -- | Conjunction of two constraints (W₁, W₂)
      Conjunction Constraint Constraint
    | -- | Equality constraint (τ₁ ~ τ₂)
      Equality Type Type
    deriving (Show, Eq)

-- TODO: Class constraints, implications

{- | Generates a list of 'Constraint's from the given 'Context'.
This generates constraints by matching annotations of identical elements
For example, if we have a context with the following elements:
@
[ x: x?
, x: Int ]
@
This will generate a single constraint:
@
x? ~ Int
@
-}
createConstraints :: Context () -> [Constraint]
createConstraints context = do
    let annotations = groupAnnotations $ mapMaybe toAnnotation context
    (name, vs) <- annotations
    let allEqual :: NonEmpty Type -> Constraint
        allEqual (x :| xs) = foldl' (\acc y -> Conjunction acc (Equality x y)) EmptyConstraint xs

    tidyConstraints $ pure $ allEqual vs

tidyConstraints :: [Constraint] -> [Constraint]
tidyConstraints = map go
  where
    go (Conjunction EmptyConstraint b) = go b
    go (Conjunction a EmptyConstraint) = go a
    go (Conjunction a b) = Conjunction (go a) (go b)
    go x = x



toAnnotation :: Entry () -> Maybe (IgnoreLocVarRef Name, Type)
toAnnotation = preview (_Ctor' @"Annotation")

groupAnnotations :: [(IgnoreLocVarRef Name, Type)] -> [(IgnoreLocVarRef Name, NonEmpty Type)]
groupAnnotations [] = []
groupAnnotations ((k, v) : xs) = (k, v :| map snd ys) : groupAnnotations zs
  where
    (ys, zs) = span ((== k) . fst) xs


rewriteConstraints :: [Constraint] -> [Constraint]
rewriteConstraints consts = 
  let rewritten = rewriteConstraintsOneStep consts
  in if rewritten == consts then rewritten else rewriteConstraints rewritten


rewriteConstraintsOneStep :: [Constraint] -> [Constraint]
rewriteConstraintsOneStep = map rewriteSingleConstraint

rewriteSingleConstraint :: Constraint -> Constraint
rewriteSingleConstraint EmptyConstraint = EmptyConstraint
