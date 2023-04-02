module Elara.TypeInfer.Unify where

import Control.Lens
import Elara.AST.Typed (PartialType (..), Type (..), Type' (..), _Id)
import Elara.Data.Unique (UniqueId)
import Elara.TypeInfer.Environment (TypeEnvironment)
import Elara.TypeInfer.Error
import Elara.TypeInfer.GenerateEquations (TypeEquation (TypeEquation))
import Elara.TypeInfer.SubstitutionMap (SubstitutionMap)
import Elara.TypeInfer.SubstitutionMap qualified as SubstitutionMap
import Polysemy
import Polysemy.Error
import Print (debugWithResult)
import Relude.Extra (secondF)
import Prelude hiding (Type)

unifyAllEquations ::
    Member (Error (TypeError, SubstitutionMap)) r =>
    [TypeEquation] ->
    TypeEnvironment ->
    SubstitutionMap ->
    Sem r SubstitutionMap
unifyAllEquations [] _ sub = pure sub
unifyAllEquations (TypeEquation (t1, t2) : eqs) env sub =
    unifyPartialTypes t1 t2 env sub >>= \sub' -> unifyAllEquations eqs env (traceShowId sub')

class TypeLike a where
    unifyType :: Member (Error (TypeError, SubstitutionMap)) r => a -> a -> TypeEnvironment -> SubstitutionMap -> Sem r SubstitutionMap
    toPartial :: a -> PartialType

instance TypeLike PartialType where
    unifyType = unifyPartialTypes
    toPartial = identity

instance TypeLike Type where
    unifyType (Type t1) (Type t2) = unifyTypes t1 t2
    toPartial = concreteToPartial

instance (Show t, TypeLike t) => TypeLike (Type' t) where
    unifyType = unifyTypes
    toPartial = Partial . fmap toPartial

unifyPartialTypes ::
    Member (Error (TypeError, SubstitutionMap)) r =>
    PartialType ->
    PartialType ->
    TypeEnvironment ->
    SubstitutionMap ->
    Sem r SubstitutionMap
unifyPartialTypes t1 t2 env substitutionMap =
    if t1 == t2
        then pure substitutionMap
        else case (t1, t2) of
            (Id id, _) -> unifyVariable id t2 env substitutionMap
            (_, Id id) -> unifyVariable id t1 env substitutionMap
            (Partial p1, Partial p2) -> unifyTypes p1 p2 env substitutionMap
            (Final (Type f1), Final (Type f2)) -> unifyTypes f1 f2 env substitutionMap
            (Final f1, p2@(Partial _)) -> unifyType (concreteToPartial f1) p2 env substitutionMap
            (p1@(Partial _), Final f2) -> unifyType p1 (concreteToPartial f2) env substitutionMap

concreteToPartial :: Type -> PartialType
concreteToPartial (Type t) = Partial (concreteToPartial' t)
  where
    concreteToPartial' :: Type' Type -> Type' PartialType
    concreteToPartial' (TypeVar v) = TypeVar v
    concreteToPartial' UnitType = UnitType
    concreteToPartial' (FunctionType a b) = FunctionType (concreteToPartial a) (concreteToPartial b)
    concreteToPartial' (UserDefinedType ut) = UserDefinedType ut
    concreteToPartial' (TypeConstructorApplication ctor args) = TypeConstructorApplication (concreteToPartial ctor) (concreteToPartial args)
    concreteToPartial' (RecordType fields) = RecordType (secondF concreteToPartial fields)

unifyTypes ::
    Member (Error (TypeError, SubstitutionMap)) r =>
    (TypeLike a, Show a) =>
    Type' a ->
    Type' a ->
    TypeEnvironment ->
    SubstitutionMap ->
    Sem r SubstitutionMap
unifyTypes t1 t2 env substitutionMap =
    let err = throw (TypeMismatch (toPartial t1) (toPartial t2), substitutionMap)
     in case (t1, t2) of
            (TypeVar v1, TypeVar v2) | v1 == v2 -> pure substitutionMap
            (TypeVar _, _) -> err
            (UnitType, UnitType) -> pure substitutionMap
            (UnitType, _) -> err
            (FunctionType a1 b1, FunctionType a2 b2) -> unifyType a1 a2 env substitutionMap >>= unifyType b1 b2 env
            (FunctionType _ _, _) -> err
            (UserDefinedType ut1, UserDefinedType ut2) | ut1 == ut2 -> pure substitutionMap
            (UserDefinedType _, _) -> err
            _ -> err

unifyVariable :: (Member (Error (TypeError, SubstitutionMap)) r) => UniqueId -> PartialType -> TypeEnvironment -> SubstitutionMap -> Sem r SubstitutionMap
unifyVariable id otherTypeOrId aliases substitutionMap =
    case SubstitutionMap.lookup id substitutionMap of
        Just t -> unifyPartialTypes t otherTypeOrId aliases substitutionMap
        Nothing ->
            case traceShowId (otherTypeOrId) ^? _Id
                >>= (`SubstitutionMap.lookup` substitutionMap)
                <&> (\typeOrId2 -> unifyVariable id typeOrId2 aliases substitutionMap) of
                Just result -> result
                Nothing ->
                    if occurs id otherTypeOrId substitutionMap
                        then error "occurs check failed"
                        else pure (SubstitutionMap.insert id otherTypeOrId substitutionMap)

occurs :: UniqueId -> PartialType -> SubstitutionMap -> Bool
occurs id typeOrId substitutionMap =
    case typeOrId of
        Id id' -> id == id'
        _ -> False