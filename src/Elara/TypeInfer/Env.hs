{-# LANGUAGE TemplateHaskell #-}

module Elara.TypeInfer.Env where

import Control.Lens (makeLenses, use)
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Elara.AST.Name
import Elara.AST.Typed
import Elara.Data.Unique (UniqueGen, freshUniqueSupply, makeUnique, uniqueGenToState)
import Polysemy
import Polysemy.Error
import Polysemy.Lens
import Polysemy.State (State, evalState, get, put)
import Relude.Extra.Bifunctor
import Prelude hiding (Constraint, Type)

newtype TypeEnv = TypeEnv (Map (VarRef VarName) Scheme) deriving (Eq)

data Scheme = Forall [TypeVar] Type
  deriving (Eq)

type Substitution = Map TypeVar Type
type Constraint = (Type, Type)

type Unifier = (Substitution, [Constraint])

data TypeError
  = UnificationFail Type Type
  | UnificationMismatch [Type] [Type]
  | InfiniteType TypeVar Type
  | LetDefConflict Type Type
  | UnboundVariable (VarRef VarName)
  | Other String
  deriving (Eq)

type Solve a = Sem '[Error TypeError] a

data InferState = InferState {_typeEnv :: TypeEnv, _typeVars :: [Text]}

makeLenses ''InferState

class Substitutable a where
  applySubstitution :: Substitution -> a -> a

  freeTypeVariables :: a -> Set TypeVar
  -- ^ Finds free type variables of a type

instance Substitutable Type where
  applySubstitution _ (UserDefinedType a) = UserDefinedType a
  applySubstitution _ UnitType = UnitType
  applySubstitution s t@(TypeVar uvn) = Map.findWithDefault t uvn s
  applySubstitution s (t1 `FunctionType` t2) = applySubstitution s t1 `FunctionType` applySubstitution s t2
  applySubstitution s (TypeConstructorApplication t1 t2) = TypeConstructorApplication (applySubstitution s t1) (applySubstitution s t2)
  applySubstitution s (RecordType fields) = RecordType (secondF (applySubstitution s) fields)

  freeTypeVariables (UserDefinedType _) = mempty
  freeTypeVariables (TypeVar uvn) = one uvn
  freeTypeVariables (t1 `FunctionType` t2) = freeTypeVariables t1 <> freeTypeVariables t2
  freeTypeVariables UnitType = mempty
  freeTypeVariables (TypeConstructorApplication t1 t2) = freeTypeVariables t1 <> freeTypeVariables t2
  freeTypeVariables (RecordType fields) = foldMap (freeTypeVariables . snd) fields

instance Substitutable Scheme where
  applySubstitution s (Forall uvs t) = Forall uvs (applySubstitution (foldr Map.delete s uvs) t)
  freeTypeVariables (Forall uvs t) = freeTypeVariables t Set.\\ Set.fromList uvs

instance Substitutable TypeEnv where
  applySubstitution s (TypeEnv env) = TypeEnv (Map.map (applySubstitution s) env)
  freeTypeVariables (TypeEnv env) = foldMap freeTypeVariables env

instance Substitutable Constraint where
  applySubstitution s (t1, t2) = (applySubstitution s t1, applySubstitution s t2)
  freeTypeVariables (t1, t2) = freeTypeVariables t1 <> freeTypeVariables t2

instance Substitutable a => Substitutable [a] where
  applySubstitution s = fmap (applySubstitution s)
  freeTypeVariables = foldMap freeTypeVariables

compose :: Substitution -> Substitution -> Substitution
s1 `compose` s2 = Map.map (applySubstitution s1) s2 `Map.union` s1

occursCheck :: Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` freeTypeVariables t

emptyEnv :: TypeEnv
emptyEnv = TypeEnv mempty

-- | Infinite list of type variable names
letters :: [Text]
letters = toText <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

closeOver :: Type -> Scheme
closeOver = normalize . generalize emptyEnv

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (fmap snd ord) (normalizeType body)
 where
  lets = run $ evalState freshUniqueSupply $ uniqueGenToState (traverse (fmap TyVar . makeUnique) letters)
  ord = zip (ordNub $ fv body) lets

  fv (TypeVar a) = [a]
  fv (t1 `FunctionType` t2) = fv t1 <> fv t2
  fv UnitType = []
  fv (TypeConstructorApplication t1 t2) = fv t1 <> fv t2
  fv (RecordType fields) = foldMap (fv . snd) fields
  fv (UserDefinedType _) = []

  normalizeType (TypeVar a) = case lookup a ord of
    Just b -> TypeVar b
    Nothing -> error "type variable not found in normalizeType"
  normalizeType (t1 `FunctionType` t2) = normalizeType t1 `FunctionType` normalizeType t2
  normalizeType UnitType = UnitType
  normalizeType (TypeConstructorApplication t1 t2) = TypeConstructorApplication (normalizeType t1) (normalizeType t2)
  normalizeType (RecordType fields) = RecordType (secondF normalizeType fields)
  normalizeType (UserDefinedType a) = UserDefinedType a

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
 where
  as = Set.toList $ freeTypeVariables t `Set.difference` freeTypeVariables env

fresh :: (Member (State InferState) r, Member UniqueGen r) => Sem r Type
fresh = do
  s <- get
  case _typeVars s of
    [] -> error "Ran out of type variables somehow"
    (tv : tvs) -> do
      put s{_typeVars = tvs}
      TypeVar . TyVar <$> makeUnique tv

instantiate :: (Member (State InferState) r, Member UniqueGen r) => Scheme -> Sem r Type
instantiate (Forall as t) = do
  as' <- traverse (const fresh) as
  let s = Map.fromList $ zip as as'
  pure $ applySubstitution s t

bind :: Member (Error TypeError) r => TypeVar -> Type -> Sem r Substitution
bind a t
  | t == TypeVar a = pure Map.empty
  | occursCheck a t = throw $ InfiniteType a t
  | otherwise = pure (one (a, t))

unifyMany :: Member (Error TypeError) r => [Type] -> [Type] -> Sem r Substitution
unifyMany [] [] = pure Map.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do
    su1 <- unifies t1 t2
    su2 <- unifyMany (applySubstitution su1 ts1) (applySubstitution su1 ts2)
    pure (su2 `compose` su1)
unifyMany t1 t2 = throw $ UnificationMismatch t1 t2

unifies :: Member (Error TypeError) r => Type -> Type -> Sem r Substitution
unifies t1 t2 | t1 == t2 = pure Map.empty
unifies (TypeVar v) t = v `bind` t -- Bind a type variable to a type
unifies t (TypeVar v) = v `bind` t -- Bind a type variable to a type
unifies (TypeConstructorApplication t1 t2) (TypeConstructorApplication t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (FunctionType t1 t2) (FunctionType t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throw $ UnificationFail t1 t2

solver :: (Member (Error TypeError) r) => Unifier -> Sem r Substitution
solver (su, cs) = case cs of
  [] -> pure su
  (t1, t2) : cs0 -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, applySubstitution su1 cs0)

runSolve :: [Constraint] -> Either TypeError Substitution
runSolve cs = run $ runError $ solver st
 where
  st = (Map.empty, cs)

lookupTypeEnv :: (Member (Error TypeError) r, Member (State InferState) r, Member UniqueGen r) => VarRef VarName -> Sem r Type
lookupTypeEnv vr = do
  TypeEnv env <- use' typeEnv
  case Map.lookup vr env of
    Just t -> instantiate t
    Nothing -> throw $ UnboundVariable vr
