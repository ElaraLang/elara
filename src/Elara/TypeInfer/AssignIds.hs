-- | Gives every sub-expression a unique ID and converts from @Elara.AST.Shunted@ to @Elara.AST.Typed@.
module Elara.TypeInfer.AssignIds where

import Elara.Data.Unique

import Control.Lens
import Elara.AST.Region
import Elara.AST.Shunted as In
import Elara.AST.Typed as Out
import Polysemy
import Relude.Extra (dup)

assignIds :: forall r. Member UniqueGen r => In.Expr -> Sem r (Out.Expr PartialType, PartialType)
assignIds (In.Expr le) = bimap Out.Expr snd . dup . unwrap <$> traverseOf unlocated assignIds' le
 where
  assignIds' :: In.Expr' -> Sem r (Out.Expr' PartialType, PartialType)
  assignIds' (In.Int i) = (Out.Int i,) <$> newId
  assignIds' (In.Float i) = (Out.Float i,) <$> newId
  assignIds' (In.String i) = (Out.String i,) <$> newId
  assignIds' (In.Char i) = (Out.Char i,) <$> newId
  assignIds' In.Unit = (Out.Unit,) <$> newId
  assignIds' (In.Var v) = (Out.Var (transformVarRef <$> v),) <$> newId
  assignIds' (In.Constructor v) = (Out.Constructor (transformVarRef <$> v),) <$> newId
  assignIds' (In.Let v e1) = do
    (e1', t1) <- assignIds e1
    pure (Out.Let v e1', t1)
  assignIds' (In.Lambda v e) = do
    (e', t) <- assignIds e
    pure (Out.Lambda v e', t)
  assignIds' (In.FunctionCall e1 e2) = do
    (e1', _) <- assignIds e1
    (e2', _) <- assignIds e2
    t <- newId
    pure (Out.FunctionCall e1' e2', t)
  assignIds' (In.If e1 e2 e3) = do
    (e1', _) <- assignIds e1
    (e2', _) <- assignIds e2
    (e3', _) <- assignIds e3
    t <- newId
    pure (Out.If e1' e2' e3', t)
  assignIds' (In.List es) = do
    es' <- fst <<$>> traverse assignIds es
    t <- newId
    pure (Out.List es', t)
  assignIds' (In.LetIn n e b) = do
    (e', _) <- assignIds e
    (b', t) <- assignIds b
    pure (Out.LetIn n e' b', t)
  assignIds' (In.Match e ps) = do
    (e', _) <- assignIds e

    ps' <- fsts <<$>> traverse (bitraverse assignIdsToPattern assignIds) ps
    t <- newId
    pure (Out.Match e' ps', t)
  assignIds' (In.Block es) = do
    es' <- fst <<$>> traverse assignIds es
    t <- newId
    pure (Out.Block es', t)

assignIdsToPattern :: forall r. Member UniqueGen r => In.Pattern -> Sem r (Out.Pattern PartialType, PartialType)
assignIdsToPattern (In.Pattern lp) = bimap Out.Pattern snd . dup . unwrap <$> traverseOf unlocated assignIdsToPattern' lp
 where
  assignIdsToPattern' :: In.Pattern' -> Sem r (Out.Pattern' PartialType, PartialType)
  assignIdsToPattern' (In.VarPattern v) = (Out.VarPattern (transformVarRef <$> v),) <$> newId
  assignIdsToPattern' (In.ConstructorPattern v as) = do
    as' <- fst <<$>> traverse assignIdsToPattern as
    t <- newId
    pure (Out.ConstructorPattern v as', t)
  assignIdsToPattern' In.WildcardPattern = (Out.WildcardPattern,) <$> newId
  assignIdsToPattern' (In.IntegerPattern i) = (Out.IntegerPattern i,) <$> newId
  assignIdsToPattern' (In.FloatPattern i) = (Out.FloatPattern i,) <$> newId
  assignIdsToPattern' (In.StringPattern i) = (Out.StringPattern i,) <$> newId
  assignIdsToPattern' (In.CharPattern i) = (Out.CharPattern i,) <$> newId
  assignIdsToPattern' (In.ListPattern ps) = do
    ps' <- fst <<$>> traverse assignIdsToPattern ps
    t <- newId
    pure (Out.ListPattern ps', t)

transformVarRef :: In.VarRef i -> Out.VarRef i
transformVarRef (In.Global n) = Out.Global n
transformVarRef (In.Local n) = Out.Local n

fsts :: ((a, b), (c, d)) -> (a, c)
fsts ((a, _), (c, _)) = (a, c)

unwrap :: Located (a, b) -> (Located a, b)
unwrap l = (fst <$> l, view (unlocated . _2) l)

newId :: Member UniqueGen r => Sem r PartialType
newId = Id <$> makeUnique ()