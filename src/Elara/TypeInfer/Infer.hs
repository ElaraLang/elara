{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | This module is based on the bidirectional type-checking algorithm from:

    Dunfield, Jana, and Neelakantan R. Krishnaswami. \"Complete and easy bidirectional typechecking for higher-rank polymorphism.\" ACM SIGPLAN Notices 48.9 (2013): 429-442.

    The main differences from the original algorithm are:

    * This uses `Control.Monad.State.Strict.StateT` to thread around
      `Context`s and manipulate them instead of explicit `Context` passing as
      in the original paper

    * This algorithm adds support for existential quantification

    * This algorithm adds support for row polymorphic and polymorphic variants
-}
module Elara.TypeInfer.Infer where

import Elara.AST.Shunted (Expr (..), mkLocal', _Expr)
import Elara.TypeInfer.Context (Context, Entry)
import Elara.TypeInfer.Existential (Existential)
import Elara.TypeInfer.Monotype (Monotype)
import Elara.TypeInfer.Type (Type (..))

import Control.Lens ((^.))
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Elara.AST.Region
import Elara.AST.Shunted qualified as Syntax
import Elara.TypeInfer.Context qualified as Context
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Error (TypeInferenceError(..))
import Elara.TypeInfer.Monotype qualified as Monotype
import Elara.TypeInfer.Type qualified as Type
import Polysemy
import Polysemy.Error
import Polysemy.State hiding (get)
import Polysemy.State qualified as State

-- | Type-checking state
data Status = Status
    { count :: !Int
    -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂ from the
    --   original paper)
    , context :: Context SourceRegion
    -- ^ The type-checking context (e.g. Γ, Δ, Θ)
    }

initialStatus :: Status
initialStatus = Status{count = 0, context = []}

orDie :: (Member (Error e) r) => Maybe a -> e -> Sem r a
Just x `orDie` _ = pure x
Nothing `orDie` e = throw e

-- | Generate a fresh existential variable (of any type)
fresh :: (Member (State Status) r) => Sem r (Existential a)
fresh = do
    Status{count = n, ..} <- State.get

    State.put $! Status{count = n + 1, ..}

    pure (fromIntegral n)

-- Unlike the original paper, we don't explicitly thread the `Context` around.
-- Instead, we modify the ambient state using the following utility functions:

-- | Push a new `Context` `Entry` onto the stack
push :: (Member (State Status) r) => Entry SourceRegion -> Sem r ()
push entry = State.modify (\s -> s{context = entry : context s})

-- | Retrieve the current `Context`
get :: (Member (State Status) r) => Sem r (Context SourceRegion)
get = gets context

-- | Set the `Context` to a new value
set :: (Member (State Status) r) => Context SourceRegion -> Sem r ()
set context = State.modify (\s -> s{context})

{- | This is used to temporarily add a `Context` entry that is discarded at the
    end of the entry's scope, along with any downstream entries that were
    created within that same scope
-}
scoped :: (Member (State Status) r) => Entry SourceRegion -> Sem r a -> Sem r a
scoped entry k = do
    push entry

    r <- k

    State.modify (\s -> s{context = Context.discardUpTo entry (context s)})

    pure r

scopedUnsolvedType :: (Member (State Status) r) => s -> (Type.Type s -> Sem r a) -> Sem r a
scopedUnsolvedType location k = do
    existential <- fresh

    scoped (Context.MarkerType existential) do
        push (Context.UnsolvedType existential)

        k Type.UnsolvedType{..}

scopedUnsolvedFields :: (Member (State Status) r) => (Type.Record s -> Sem r a) -> Sem r a
scopedUnsolvedFields k = do
    a <- fresh

    scoped (Context.MarkerFields a) do
        push (Context.UnsolvedFields a)

        k (Type.Fields [] (Monotype.UnsolvedFields a))

scopedUnsolvedAlternatives :: (Member (State Status) r) => (Type.Union s -> Sem r a) -> Sem r a
scopedUnsolvedAlternatives k = do
    a <- fresh

    scoped (Context.MarkerAlternatives a) do
        push (Context.UnsolvedAlternatives a)

        k (Type.Alternatives [] (Monotype.UnsolvedAlternatives a))

{- | This corresponds to the judgment:

    > Γ ⊢ A

    … which checks that under context Γ, the type A is well-formed
-}
wellFormedType ::
    (Member (Error TypeInferenceError) r) =>
    Context SourceRegion ->
    Type SourceRegion ->
    Sem r ()
wellFormedType _Γ type0 =
    case type0 of
        -- UvarWF
        Type.VariableType{..}
            | Context.Variable Domain.Type name `elem` _Γ -> do
                pure ()
            | otherwise -> throw (UnboundTypeVariable location name)
        -- ArrowWF
        Type.Function{..} -> do
            wellFormedType _Γ input
            wellFormedType _Γ output

        -- ForallWF
        Type.Forall{..} -> do
            wellFormedType (Context.Variable domain name : _Γ) type_

        -- ForallWF
        Type.Exists{..} -> do
            wellFormedType (Context.Variable domain name : _Γ) type_

        -- EvarWF / SolvedEvarWF
        _A@Type.UnsolvedType{..}
            | any predicate _Γ -> do
                pass
            | otherwise -> do
                throw (IllFormedType location _A _Γ)
          where
            predicate (Context.UnsolvedType a) = existential == a
            predicate (Context.SolvedType a _) = existential == a
            predicate _ = False
        Type.Optional{..} -> do
            wellFormedType _Γ type_
        Type.List{..} -> do
            wellFormedType _Γ type_
        Type.Tuple{..} -> do
            traverse_ (wellFormedType _Γ) types
        Type.Record{fields = Type.Fields kAs Monotype.EmptyFields} -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
        Type.Record{fields = Type.Fields kAs (Monotype.UnsolvedFields a0), ..}
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throw (IllFormedFields location a0 _Γ)
          where
            predicate (Context.UnsolvedFields a1) = a0 == a1
            predicate (Context.SolvedFields a1 _) = a0 == a1
            predicate _ = False
        Type.Record{fields = Type.Fields kAs (Monotype.VariableFields a), ..}
            | Context.Variable Domain.Fields a `elem` _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throw (UnboundFields location a)
        Type.Union{alternatives = Type.Alternatives kAs Monotype.EmptyAlternatives} -> do
            traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
        Type.Union{alternatives = Type.Alternatives kAs (Monotype.UnsolvedAlternatives a0), ..}
            | any predicate _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throw (IllFormedAlternatives location a0 _Γ)
          where
            predicate (Context.UnsolvedAlternatives a1) = a0 == a1
            predicate (Context.SolvedAlternatives a1 _) = a0 == a1
            predicate _ = False
        Type.Union{alternatives = Type.Alternatives kAs (Monotype.VariableAlternatives a), ..}
            | Context.Variable Domain.Alternatives a `elem` _Γ -> do
                traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
            | otherwise -> do
                throw (UnboundAlternatives location a)
        Type.Scalar{} -> do
            pure ()

{- | This corresponds to the judgment:

    > Γ ⊢ A <: B ⊣ Δ

    … which updates the context Γ to produce the new context Δ, given that the
    type A is a subtype of type B.
-}
subtype ::
    (HasCallStack) =>
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Type SourceRegion ->
    Type SourceRegion ->
    Sem r ()
subtype _A0 _B0 = do
    _Γ <- get

    case (_A0, _B0) of
        -- <:Var
        (Type.VariableType{name = a0}, Type.VariableType{name = a1})
            | a0 == a1 -> do
                wellFormedType _Γ _A0

        -- <:Exvar
        (Type.UnsolvedType{existential = a0}, Type.UnsolvedType{existential = a1})
            | a0 == a1 && Context.UnsolvedType a0 `elem` _Γ -> do
                pure ()

        -- InstantiateL
        (Type.UnsolvedType{existential = a}, _)
            -- The `not (a `Type.typeFreeIn` _B)` is the "occurs check" which
            -- prevents a type variable from being defined in terms of itself
            -- (i.e. a type should not "occur" within itself).
            --
            -- Later on you'll see matching "occurs checks" for record types and
            -- union types so that Fields variables and Alternatives variables
            -- cannot refer to the record or union that they belong to,
            -- respectively.
            | not (a `Type.typeFreeIn` _B0)
                && elem (Context.UnsolvedType a) _Γ -> do
                instantiateTypeL a _B0

        -- InstantiateR
        (_, Type.UnsolvedType{existential = a})
            | not (a `Type.typeFreeIn` _A0)
                && elem (Context.UnsolvedType a) _Γ -> do
                instantiateTypeR _A0 a

        -- <:→
        (Type.Function{input = _A1, output = _A2}, Type.Function{input = _B1, output = _B2}) -> do
            subtype _B1 _A1

            _Θ <- get

            -- CAREFULLY NOTE: Pay really close attention to how we need to use
            -- `Context.solveType` any time we update the context.  The paper
            -- already mentions this, but if you forget to do this then you
            -- will get bugs due to unsolved variables not getting solved
            -- correctly.
            --
            -- A much more reliable way to fix this problem would simply be to
            -- have every function (like `subtype`, `instantiateL`, …)
            -- apply `solveType` to its inputs.  For example, this very
            -- `subtype` function could begin by doing:
            --
            --     _Γ <- get
            --     let _A0' = Context.solveType _Γ _A0
            --     let _B0' = Context.solveType _Γ _B0
            --
            -- … and then use _A0' and _B0' for downstream steps.  If we did
            -- that at the beginning of each function then everything would
            -- "just work".
            --
            -- However, this would be more inefficient because we'd calling
            -- `solveType` wastefully over and over with the exact same context
            -- in many cases.  So, the tradeoff here is that we get improved
            -- performance if we're willing to remember to call `solveType` in
            -- the right places.
            subtype (Context.solveType _Θ _A2) (Context.solveType _Θ _B2)

        -- One of the main extensions that is not present in the original paper
        -- is the addition of existential quantification.  This was actually
        -- pretty easy to implement: you just take the rules for universal
        -- quantification and flip them around and everything works.  Elegant!
        --
        -- For example, the <:∃R rule is basically the same as the <:∀L rule,
        -- except with the arguments flipped.  Similarly, the <:∃L rule is
        -- basically the same as the <:∀R rule with the arguments flipped.

        -- <:∃R
        (_, Type.Exists{domain = Domain.Type, ..}) -> do
            scopedUnsolvedType nameLocation \a ->
                subtype _A0 (Type.substituteType name 0 a type_)
        (_, Type.Exists{domain = Domain.Fields, ..}) -> do
            scopedUnsolvedFields \a -> do
                subtype _A0 (Type.substituteFields name 0 a type_)
        (_, Type.Exists{domain = Domain.Alternatives, ..}) -> do
            scopedUnsolvedAlternatives \a -> do
                subtype _A0 (Type.substituteAlternatives name 0 a type_)

        -- <:∀L
        (Type.Forall{domain = Domain.Type, ..}, _) -> do
            scopedUnsolvedType nameLocation \a -> do
                subtype (Type.substituteType name 0 a type_) _B0
        (Type.Forall{domain = Domain.Fields, ..}, _) -> do
            scopedUnsolvedFields \a -> do
                subtype (Type.substituteFields name 0 a type_) _B0
        (Type.Forall{domain = Domain.Alternatives, ..}, _) -> do
            scopedUnsolvedAlternatives \a -> do
                subtype (Type.substituteAlternatives name 0 a type_) _B0

        -- <:∃L
        (Type.Exists{..}, _) -> do
            scoped (Context.Variable domain name) do
                subtype type_ _B0

        -- <:∀R
        (_, Type.Forall{..}) -> do
            scoped (Context.Variable domain name) do
                subtype _A0 type_
        (Type.Scalar{scalar = s0}, Type.Scalar{scalar = s1})
            | s0 == s1 -> do
                pure ()
        (Type.Optional{type_ = _A}, Type.Optional{type_ = _B}) -> do
            subtype _A _B
        (Type.List{type_ = _A}, Type.List{type_ = _B}) -> do
            subtype _A _B
        (Type.Tuple{types = typesA}, Type.Tuple{types = typesB}) -> do
            when (length typesA /= length typesB) do
                error "Tuple types must have the same number of elements"

            for_ (NE.zip typesA typesB) \(a, b) -> do
                subtype a b

        -- This is where you need to add any non-trivial subtypes.  For example,
        -- the following three rules specify that `Natural` is a subtype of
        -- `Integer`, which is in turn a subtype of `Real`.
        (Type.Scalar{scalar = Monotype.Natural}, Type.Scalar{scalar = Monotype.Integer}) -> do
            pass
        (Type.Scalar{scalar = Monotype.Natural}, Type.Scalar{scalar = Monotype.Real}) -> do
            pass
        (Type.Scalar{scalar = Monotype.Integer}, Type.Scalar{scalar = Monotype.Real}) -> do
            pass

        -- Similarly, this is the rule that says that `T` is a subtype of
        -- `Optional T`.  If that feels unprincipled to you then delete this
        -- rule.
        (_, Type.Optional{..}) -> do
            subtype _A0 type_

        -- The type-checking code for records is the first place where we
        -- implement a non-trivial type that wasn't already covered by the
        -- paper, so we'll go into more detail here to explain the general
        -- type-checking principles of the paper.
        (_A@Type.Record{fields = Type.Fields kAs0 fields0}, _B@Type.Record{fields = Type.Fields kBs0 fields1}) -> do
            let mapA = Map.fromList kAs0
            let mapB = Map.fromList kBs0

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let flexible Monotype.EmptyFields = False
                flexible (Monotype.VariableFields _) = False
                flexible (Monotype.UnsolvedFields _) = True

            let okayA =
                    Map.null extraA
                        || (flexible fields1 && fields0 /= fields1)

            let okayB =
                    Map.null extraB
                        || (flexible fields0 && fields0 /= fields1)

            -- First we check that there are no mismatches in the record types
            -- that cannot be resolved by just setting an unsolved Fields
            -- variable to the right type.
            --
            -- For example, `{ x: Bool }` can never be a subtype of
            -- `{ y: Text }`
            if
                    | not okayA && not okayB -> do
                        throw (RecordTypeMismatch _A0 _B0 extraA extraB)
                    | not okayA -> do
                        throw (RecordTypeMismatch _A0 _B0 extraA mempty)
                    | not okayB -> do
                        throw (RecordTypeMismatch _A0 _B0 mempty extraB)
                    | otherwise -> do
                        pure ()

            -- If record A is a subtype of record B, then all fields in A
            -- must be a subtype of the matching fields in record B
            let process (_A1, _B1) = do
                    _Θ <- get

                    subtype
                        (Context.solveType _Θ _A1)
                        (Context.solveType _Θ _B1)

            -- We only check fields are present in `both` records.  For
            -- mismatched fields present only in one record type we have to
            -- skip to the next step of resolving the mismatch by solving Fields
            -- variables.
            traverse_ process both

            -- Here is where we handle fields that were only present in one
            -- record type.  They still might be okay if one or both of the
            -- record types has an unsolved fields variable.
            case (fields0, fields1) of
                -- The two records are identical, so there's nothing left to do
                _ | null extraA && null extraB && fields0 == fields1 -> do
                    pure ()

                -- Both records type have unsolved Fields variables.  Great!
                -- This is the most flexible case, since we can replace these
                -- unsolved variables with whatever fields we want to make the
                -- types match.
                --
                -- However, it's not as simple as setting each Fields variable
                -- to the extra fields from the opposing record type.  For
                -- example, if the two record types we're comparing are:
                --
                --     { x: Bool, p0 } <: { y: Text, p1 }
                --
                -- … then it's not correct to say:
                --
                --     p0 = y: Text
                --     p1 = x: Bool
                --
                -- … because that is not the most general solution for `p0` and
                -- `p1`!  The actual most general solution is:
                --
                --     p0 = { y: Text, p2 }
                --     p1 = { x: Bool, p2 }
                --
                -- … where `p2` is a fresh Fields type variable representing the
                -- fact that both records could potentially have even more
                -- fields other than `x` and `y`.
                (Monotype.UnsolvedFields p0, Monotype.UnsolvedFields p1) -> do
                    p2 <- fresh

                    _Γ0 <- get

                    -- We have to insert p2 before both p0 and p1 within the
                    -- context because the bidirectional type-checking algorithm
                    -- requires that the context is ordered and all variables
                    -- within the context can only reference prior variables
                    -- within the context.
                    --
                    -- Since `p0` and `p1` both have to reference `p2`, then we
                    -- need to insert `p2` right before `p0` or `p1`, whichever
                    -- one comes first
                    let p0First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ0

                            Monad.guard (Context.UnsolvedFields p1 `elem` _ΓR)

                            let command =
                                    set
                                        ( _ΓR
                                            <> ( Context.UnsolvedFields p0
                                                    : Context.UnsolvedFields p2
                                                    : _ΓL
                                               )
                                        )

                            pure command

                    let p1First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p1 _Γ0

                            Monad.guard (Context.UnsolvedFields p0 `elem` _ΓR)

                            let command =
                                    set
                                        ( _ΓR
                                            <> ( Context.UnsolvedFields p1
                                                    : Context.UnsolvedFields p2
                                                    : _ΓL
                                               )
                                        )

                            pure command

                    case p0First <|> p1First of
                        Nothing -> do
                            throw (MissingOneOfFields [Type.location _A0, Type.location _B0] p0 p1 _Γ)
                        Just setContext -> do
                            setContext

                    _Θ <- get

                    -- Now we solve for `p0`.  This is basically saying:
                    --
                    -- p0 = { extraFieldsFromRecordB, p2 }
                    instantiateFieldsL
                        p0
                        (Type.location _B0)
                        ( Context.solveRecord
                            _Θ
                            ( Type.Fields
                                (Map.toList extraB)
                                (Monotype.UnsolvedFields p2)
                            )
                        )

                    _Δ <- get

                    -- Similarly, solve for `p1`.  This is basically saying:
                    --
                    -- p1 = { extraFieldsFromRecordA, p2 }
                    instantiateFieldsR
                        (Type.location _A0)
                        ( Context.solveRecord
                            _Δ
                            ( Type.Fields
                                (Map.toList extraA)
                                (Monotype.UnsolvedFields p2)
                            )
                        )
                        p1

                -- If only one of the records has a Fields variable then the
                -- solution is simpler: just set the Fields variable to the
                -- extra fields from the opposing record
                (Monotype.UnsolvedFields p0, _) -> do
                    _Θ <- get

                    instantiateFieldsL
                        p0
                        (Type.location _B0)
                        ( Context.solveRecord
                            _Θ
                            (Type.Fields (Map.toList extraB) fields1)
                        )
                (_, Monotype.UnsolvedFields p1) -> do
                    _Θ <- get

                    instantiateFieldsR
                        (Type.location _A0)
                        ( Context.solveRecord
                            _Θ
                            (Type.Fields (Map.toList extraA) fields0)
                        )
                        p1
                (_, _) -> do
                    throw (NotRecordSubtype (Type.location _A0) _A (Type.location _B0) _B)

        -- Checking if one union is a subtype of another union is basically the
        -- exact same as the logic for checking if a record is a subtype of
        -- another record.
        (_A@Type.Union{alternatives = Type.Alternatives kAs0 alternatives0}, _B@Type.Union{alternatives = Type.Alternatives kBs0 alternatives1}) -> do
            let mapA = Map.fromList kAs0
            let mapB = Map.fromList kBs0

            let extraA = Map.difference mapA mapB
            let extraB = Map.difference mapB mapA

            let both = Map.intersectionWith (,) mapA mapB

            let flexible Monotype.EmptyAlternatives = False
                flexible (Monotype.VariableAlternatives _) = False
                flexible (Monotype.UnsolvedAlternatives _) = True

            let okayA =
                    Map.null extraA
                        || (flexible alternatives1 && alternatives0 /= alternatives1)
            let okayB =
                    Map.null extraB
                        || (flexible alternatives0 && alternatives0 /= alternatives1)

            if
                    | not okayA && not okayB -> do
                        throw (UnionTypeMismatch _A0 _B0 extraA extraB)
                    | not okayA && okayB -> do
                        throw (UnionTypeMismatch _A0 _B0 extraA mempty)
                    | okayA && not okayB -> do
                        throw (UnionTypeMismatch _A0 _B0 mempty extraB)
                    | otherwise -> do
                        pure ()

            let process (_A1, _B1) = do
                    _Θ <- get

                    subtype
                        (Context.solveType _Θ _A1)
                        (Context.solveType _Θ _B1)

            traverse_ process both

            case (alternatives0, alternatives1) of
                (Monotype.UnsolvedAlternatives p0, Monotype.UnsolvedAlternatives p1) -> do
                    p2 <- fresh

                    _Γ0 <- get

                    let p0First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ0

                            guard (Context.UnsolvedAlternatives p1 `elem` _ΓR)

                            let command =
                                    set
                                        ( _ΓR
                                            <> ( Context.UnsolvedAlternatives p0
                                                    : Context.UnsolvedAlternatives p2
                                                    : _ΓL
                                               )
                                        )

                            pure command

                    let p1First = do
                            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p1 _Γ0

                            guard (Context.UnsolvedAlternatives p0 `elem` _ΓR)

                            let command =
                                    set
                                        ( _ΓR
                                            <> ( Context.UnsolvedAlternatives p1
                                                    : Context.UnsolvedAlternatives p2
                                                    : _ΓL
                                               )
                                        )

                            pure command

                    case p0First <|> p1First of
                        Nothing -> do
                            throw (MissingOneOfAlternatives [Type.location _A0, Type.location _B0] p0 p1 _Γ)
                        Just setContext -> do
                            setContext

                    _Θ <- get

                    instantiateAlternativesL
                        p0
                        (Type.location _B0)
                        ( Context.solveUnion
                            _Θ
                            ( Type.Alternatives
                                (Map.toList extraB)
                                (Monotype.UnsolvedAlternatives p2)
                            )
                        )

                    _Δ <- get

                    instantiateAlternativesR
                        (Type.location _A0)
                        ( Context.solveUnion
                            _Δ
                            ( Type.Alternatives
                                (Map.toList extraA)
                                (Monotype.UnsolvedAlternatives p2)
                            )
                        )
                        p1
                (Monotype.EmptyAlternatives, Monotype.EmptyAlternatives) -> do
                    pure ()
                (Monotype.UnsolvedAlternatives p0, _) -> do
                    _Θ <- get

                    instantiateAlternativesL
                        p0
                        (Type.location _B0)
                        ( Context.solveUnion
                            _Θ
                            ( Type.Alternatives
                                (Map.toList extraB)
                                alternatives1
                            )
                        )
                (Monotype.VariableAlternatives p0, Monotype.VariableAlternatives p1)
                    | p0 == p1 -> do
                        pass
                (_, Monotype.UnsolvedAlternatives p1) -> do
                    _Θ <- get

                    instantiateAlternativesR
                        (Type.location _A0)
                        ( Context.solveUnion
                            _Θ
                            ( Type.Alternatives
                                (Map.toList extraA)
                                alternatives0
                            )
                        )
                        p1
                (_, _) -> do
                    throw (NotUnionSubtype (Type.location _A0) _A (Type.location _B0) _B)

        -- Unfortunately, we need to have this wildcard match at the end,
        -- otherwise we'd have to specify a number of cases that is quadratic
        -- in the number of `Type` constructors.  That in turn means that you
        -- can easily forget to add cases like:
        --
        --     (Type.List _A, Type.List _B) -> do
        --         subtype _A _B
        --
        -- … because the exhaustivity checker won't warn you if you forget to
        -- add that case.
        --
        -- The way I remember to do this is that when I add new complex types I
        -- grep the codebase for all occurrences of an existing complex type
        -- (like `List`), and then one of the occurrences will be here in this
        -- `subtype` function and then I'll remember to add a case for my new
        -- complex type here.
        (_A, _B) -> do
            -- error "not subtype"
            throw (NotSubtype (Type.location _A0) _A (Type.location _B0) _B)

{- | This corresponds to the judgment:

    > Γ ⊢ α̂ :≦ A ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that α̂ <: A.

    The @instantiate*@ family of functions should really be called @solve*@
    because their job is to solve an unsolved variable within the context.
    However, for consistency with the paper we still name them @instantiate*@.
-}
instantiateTypeL ::
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Existential Monotype ->
    Type SourceRegion ->
    Sem r ()
instantiateTypeL a _A0 = do
    _Γ0 <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType a _Γ0 `orDie` MissingVariable a _Γ0

    let instLSolve τ = do
            wellFormedType _Γ _A0

            set (_Γ' <> (Context.SolvedType a τ : _Γ))

    case _A0 of
        -- InstLReach
        Type.UnsolvedType{..}
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType existential _Γ' -> do
                set (_ΓR <> (Context.SolvedType existential (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _ΓL))

        -- InstLSolve
        Type.UnsolvedType{..} -> do
            instLSolve (Monotype.UnsolvedType existential)
        Type.VariableType{..} -> do
            instLSolve (Monotype.VariableType name)
        Type.Scalar{..} -> do
            instLSolve (Monotype.Scalar scalar)

        -- InstLExt
        Type.Exists{domain = Domain.Type, ..} -> do
            scopedUnsolvedType nameLocation \b -> do
                instantiateTypeR (Type.substituteType name 0 b type_) a
        Type.Exists{domain = Domain.Fields, ..} -> do
            scopedUnsolvedFields \b -> do
                instantiateTypeR (Type.substituteFields name 0 b type_) a
        Type.Exists{domain = Domain.Alternatives, ..} -> do
            scopedUnsolvedAlternatives \b -> do
                instantiateTypeR (Type.substituteAlternatives name 0 b type_) a

        -- InstLArr
        Type.Function{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh
            a2 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

            instantiateTypeR input a1

            _Θ <- get

            instantiateTypeL a2 (Context.solveType _Θ output)

        -- InstLAllR
        Type.Forall{..} -> do
            scoped (Context.Variable domain name) do
                instantiateTypeL a type_

        -- This case is the first example of a general pattern we have to
        -- follow when solving unsolved variables.
        --
        -- Typically when you solve an unsolved variable (e.g. `a`) to some
        -- type (e.g. `A`), you cannot just directly solve the variable as:
        --
        --     a = A
        --
        -- … because unsolved variables can only be solved to `Monotype`s, but
        -- `A` is typically a `Type`.
        --
        -- So, instead, what you do is you solve the variable one layer at a
        -- time.  For example, if you try to solve `a` to (the `Type`)
        -- `Optional (List Bool)`, you will actually get three solved variables
        -- added to the context:
        --
        --     a = Optional b
        --     b = List c
        --     c = Bool
        --
        -- In other words, each time you solve one layer of a complex type, you
        -- need to create a fresh unsolved variable for each inner type and
        -- solve each inner unsolved variable.
        --
        -- This may seem really indirect and tedious, but if you try to skip
        -- this one-layer-at-a-time solving process then you will likely get
        -- bugs due to solved variables referring to each other out of order.
        --
        -- This wasn't obvious to me from reading the original paper since they
        -- didn't really cover how to type-check complex types other than
        -- function types.
        Type.Optional{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            -- To solve `a` against `Optional _A` we create a fresh unsolved
            -- variable named `a1`, …
            a1 <- fresh

            -- … solve `a` to `Optional a1`, taking care that `a1` comes before
            -- `a` within the context, (since `a` refers to `a1`)  …
            set (_ΓR <> (Context.SolvedType a (Monotype.Optional (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            -- … and then solve `a1` against _A`
            instantiateTypeL a1 type_

        -- We solve an unsolved variable against `List` using the same
        -- principles described above for solving `Optional`
        Type.List{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.List (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeL a1 type_
        -- Tuples are basically treated as records with fields '_1_', '_2_', ...
        Type.Tuple{types} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields p))) : Context.UnsolvedFields p : _ΓL))

            let fields = NE.zipWith (\i type_ -> ("_" <> show i, type_)) [1 ..] types
            instantiateFieldsL p (Type.location _A0) (Type.Fields (toList fields) Monotype.EmptyFields)

        -- This is still the same one-layer-at-a-time principle, with a small
        -- twist.  In order to solve:
        --
        --     a = { r }
        --
        -- We replace `r` with a new unsolved Fields variable and then solve for
        -- that Fields variable.
        Type.Record{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields p))) : Context.UnsolvedFields p : _ΓL))

            instantiateFieldsL p (Type.location _A0) fields

        -- Same principle as for `Record`, but replacing the Field variable with
        -- an Alternatives variable
        Type.Union{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Union (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p))) : Context.UnsolvedAlternatives p : _ΓL))

            instantiateAlternativesL p (Type.location _A0) alternatives

{- | This corresponds to the judgment:

    > Γ ⊢ A ≦: α̂ ⊣ Δ

    … which updates the context Γ to produce the new context Δ, by instantiating
    α̂ such that A :< α̂.
-}
instantiateTypeR ::
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Type SourceRegion ->
    Existential Monotype ->
    Sem r ()
instantiateTypeR _A0 a = do
    _Γ0 <- get

    (_Γ', _Γ) <- Context.splitOnUnsolvedType a _Γ0 `orDie` MissingVariable a _Γ0

    let instRSolve τ = do
            wellFormedType _Γ _A0

            set (_Γ' <> (Context.SolvedType a τ : _Γ))

    case _A0 of
        -- InstRReach
        Type.UnsolvedType{..}
            | let _ΓL = _Γ
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType existential _Γ' -> do
                set (_ΓR <> (Context.SolvedType existential (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _ΓL))

        -- InstRSolve
        Type.UnsolvedType{..} -> do
            instRSolve (Monotype.UnsolvedType existential)
        Type.VariableType{..} -> do
            instRSolve (Monotype.VariableType name)
        Type.Scalar{..} -> do
            instRSolve (Monotype.Scalar scalar)

        -- InstRArr
        Type.Function{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh
            a2 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

            instantiateTypeL a1 input

            _Θ <- get

            instantiateTypeR (Context.solveType _Θ output) a2

        -- InstRExtL
        Type.Exists{..} -> do
            scoped (Context.Variable domain name) do
                instantiateTypeL a type_

        -- InstRAllL
        Type.Forall{domain = Domain.Type, ..} -> do
            scopedUnsolvedType nameLocation \b -> do
                instantiateTypeR (Type.substituteType name 0 b type_) a
        Type.Forall{domain = Domain.Fields, ..} -> do
            scopedUnsolvedFields \b -> do
                instantiateTypeR (Type.substituteFields name 0 b type_) a
        Type.Forall{domain = Domain.Alternatives, ..} -> do
            scopedUnsolvedAlternatives \b -> do
                instantiateTypeR (Type.substituteAlternatives name 0 b type_) a
        Type.Optional{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Optional (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeR type_ a1
        Type.List{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1 <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.List (Monotype.UnsolvedType a1)) : Context.UnsolvedType a1 : _ΓL))

            instantiateTypeR type_ a1
        Type.Tuple{types, ..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields p))) : Context.UnsolvedFields p : _ΓL))

            let fields = NE.zipWith (\i type_ -> ("_" <> show i, type_)) [1 ..] types
            instantiateFieldsR (Type.location _A0) (Type.Fields (toList fields) Monotype.EmptyFields) p
        Type.Record{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Record (Monotype.Fields [] (Monotype.UnsolvedFields p))) : Context.UnsolvedFields p : _ΓL))

            instantiateFieldsR (Type.location _A0) fields p
        Type.Union{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            p <- fresh

            set (_ΓR <> (Context.SolvedType a (Monotype.Union (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p))) : Context.UnsolvedAlternatives p : _ΓL))

            instantiateAlternativesR (Type.location _A0) alternatives p

{- The following `equateFields` / `instantiateFieldsL` / `instantiateFieldsR`,
   `equateAlternatives` / `instantiateAlternativesL` /
   `instantiateAlternativesR` judgments are not present in the bidirectional
   type-checking paper.  These were added in order to support row polymorphism
   and variant polymorphism, by following the same general type-checking
   principles as the original paper.

   If you understand how the `instantiateTypeL` and `instantiateTypeR` functions
   work, then you will probably understand how these functions work because they
   follow the same rules:

   * Always make sure that solved variables only reference variables earlier
     within the context

   * Solve for unsolved variables one layer at a time

   Note that the implementation and the user-facing terminology use the term
   fields/alternatives instead of rows/variants, respectively.
-}

equateFields ::
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Existential Monotype.Record ->
    Existential Monotype.Record ->
    Sem r ()
equateFields p0 p1 = do
    _Γ0 <- get

    let p0First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p1 _Γ0

            Monad.guard (Context.UnsolvedFields p0 `elem` _ΓL)

            pure (set (_ΓR <> (Context.SolvedFields p1 (Monotype.Fields [] (Monotype.UnsolvedFields p0)) : _ΓL)))

    let p1First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ0

            Monad.guard (Context.UnsolvedFields p1 `elem` _ΓL)

            pure (set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields [] (Monotype.UnsolvedFields p1)) : _ΓL)))

    case p0First <|> p1First of
        Nothing -> do
            throw (MissingOneOfFields [] p0 p1 _Γ0)
        Just setContext -> do
            setContext

instantiateFieldsL ::
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Existential Monotype.Record ->
    SourceRegion ->
    Type.Record SourceRegion ->
    Sem r ()
instantiateFieldsL p0 location fields@(Type.Fields kAs rest) = do
    when (p0 `Type.fieldsFreeIn` Type.Record{..}) do
        throw (NotFieldsSubtype location p0 fields)

    let process (k, _A) = do
            b <- fresh

            pure (k, _A, b)

    kAbs <- traverse process kAs

    let bs = map (\(_, _, b) -> Context.UnsolvedType b) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ `orDie` MissingAllFields p0 _Γ

    case rest of
        Monotype.UnsolvedFields p1 -> do
            p2 <- fresh

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs (Monotype.UnsolvedFields p2)) : Context.UnsolvedFields p2 : bs <> _ΓL))

            equateFields p1 p2
        _ -> do
            wellFormedType
                (bs <> _ΓL)
                Type.Record{fields = Type.Fields [] rest, ..}

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeL b (Context.solveType _Θ _A)

    traverse_ instantiate kAbs

instantiateFieldsR ::
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    SourceRegion ->
    Type.Record SourceRegion ->
    Existential Monotype.Record ->
    Sem r ()
instantiateFieldsR location fields@(Type.Fields kAs rest) p0 = do
    when (p0 `Type.fieldsFreeIn` Type.Record{..}) do
        throw (NotFieldsSubtype location p0 fields)

    let process (k, _A) = do
            b <- fresh

            pure (k, _A, b)

    kAbs <- traverse process kAs

    let bs = map (\(_, _, b) -> Context.UnsolvedType b) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ `orDie` MissingAllFields p0 _Γ

    case rest of
        Monotype.UnsolvedFields p1 -> do
            p2 <- fresh

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs (Monotype.UnsolvedFields p2)) : Context.UnsolvedFields p2 : bs <> _ΓL))

            equateFields p1 p2
        _ -> do
            wellFormedType
                (bs <> _ΓL)
                Type.Record{fields = Type.Fields [] rest, ..}

            set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A) b

    traverse_ instantiate kAbs

equateAlternatives ::
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Existential Monotype.Union ->
    Existential Monotype.Union ->
    Sem r ()
equateAlternatives p0 p1 = do
    _Γ0 <- get

    let p0First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p1 _Γ0

            Monad.guard (Context.UnsolvedAlternatives p0 `elem` _ΓL)

            pure (set (_ΓR <> (Context.SolvedAlternatives p1 (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p0)) : _ΓL)))

    let p1First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ0

            guard (Context.UnsolvedAlternatives p1 `elem` _ΓL)

            pure (set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p1)) : _ΓL)))

    case p0First <|> p1First of
        Nothing -> do
            throw (MissingOneOfAlternatives [] p0 p1 _Γ0)
        Just setContext -> do
            setContext

instantiateAlternativesL ::
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Existential Monotype.Union ->
    SourceRegion ->
    Type.Union SourceRegion ->
    Sem r ()
instantiateAlternativesL p0 location alternatives@(Type.Alternatives kAs rest) = do
    when (p0 `Type.alternativesFreeIn` Type.Union{..}) do
        throw (NotAlternativesSubtype location p0 alternatives)

    let process (k, _A) = do
            b <- fresh

            pure (k, _A, b)

    kAbs <- traverse process kAs

    let bs = map (\(_, _, b) -> Context.UnsolvedType b) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ `orDie` MissingAllAlternatives p0 _Γ

    case rest of
        Monotype.UnsolvedAlternatives p1 -> do
            p2 <- fresh

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs (Monotype.UnsolvedAlternatives p2)) : Context.UnsolvedAlternatives p2 : bs <> _ΓL))

            equateAlternatives p1 p2
        _ -> do
            wellFormedType
                (bs <> _ΓL)
                Type.Union{alternatives = Type.Alternatives [] rest, ..}

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeL b (Context.solveType _Θ _A)

    traverse_ instantiate kAbs

instantiateAlternativesR ::
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    SourceRegion ->
    Type.Union SourceRegion ->
    Existential Monotype.Union ->
    Sem r ()
instantiateAlternativesR location alternatives@(Type.Alternatives kAs rest) p0 = do
    when (p0 `Type.alternativesFreeIn` Type.Union{..}) do
        throw (NotAlternativesSubtype location p0 alternatives)

    let process (k, _A) = do
            b <- fresh

            pure (k, _A, b)

    kAbs <- traverse process kAs

    let bs = map (\(_, _, b) -> Context.UnsolvedType b) kAbs
    let kbs = map (\(k, _, b) -> (k, Monotype.UnsolvedType b)) kAbs

    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ `orDie` MissingAllAlternatives p0 _Γ

    case rest of
        Monotype.UnsolvedAlternatives p1 -> do
            p2 <- fresh

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs (Monotype.UnsolvedAlternatives p2)) : Context.UnsolvedAlternatives p2 : bs <> _ΓL))

            equateAlternatives p1 p2
        _ -> do
            wellFormedType
                (bs <> _ΓL)
                Type.Union{alternatives = Type.Alternatives [] rest, ..}

            set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives kbs rest) : bs <> _ΓL))

    let instantiate (_, _A, b) = do
            _Θ <- get

            instantiateTypeR (Context.solveType _Θ _A) b

    traverse_ instantiate kAbs

infer' ::
    (HasCallStack) =>
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Expr ->
    Sem r (Type SourceRegion)
infer' e = fst <$> infer e pass

{- | This corresponds to the judgment:

    > Γ ⊢ e ⇒ A ⊣ Δ

    … which infers the type of e under input context Γ, producing an inferred
    type of A and an updated context Δ.
-}
infer ::
    (HasCallStack, Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Expr ->
    -- | An inner computation that can use any locally scoped context (eg lambda parameters)
    Sem r a ->
    Sem r (Type SourceRegion, a)
infer (Expr (Located location e0)) cont = do
    case e0 of
        -- Var
        Syntax.Var vn -> do
            _Γ <- get

            let n = Syntax.withName' (vn ^. unlocated)
            l <- Context.lookup n _Γ `orDie` UnboundVariable n
            c <- cont
            pure (l, c)

        -- Constructor
        Syntax.Constructor ctorName -> do
            _Γ <- get

            let n = Syntax.withName' (ctorName ^. unlocated)
            l <- Context.lookup n _Γ `orDie` UnboundConstructor n
            c <- cont
            pure (l, c)
        -- →I⇒
        Syntax.Lambda binding body -> do
            a <- fresh
            b <- fresh

            let input = Type.UnsolvedType{existential = a, ..}

            let output = Type.UnsolvedType{existential = b, ..}

            push (Context.UnsolvedType a)
            push (Context.UnsolvedType b)

            c' <- scoped (Context.Annotation (mkLocal' binding) input) do
                check body output
                cont

            pure (Type.Function{input = input, output = output, ..}, c')

        -- →E
        Syntax.FunctionCall function argument -> do
            _A <- fst <$> infer function cont

            _Θ <- get

            i <- inferApplication (Context.solveType _Θ _A) argument
            c <- cont
            pure (i, c)

        -- Anno
        -- Syntax.Annotation{..} -> do
        --     _Γ <- get

        --     wellFormedType _Γ annotation

        --     check annotated annotation

        --     pure annotation
        Syntax.LetIn name val body -> do
            _A <- fst <$> infer val cont
            push (Context.Annotation (mkLocal' name) _A)
            infer body cont
        Syntax.Let name val -> do
            (_A, c) <- infer val cont
            push (Context.Annotation (mkLocal' name) _A)
            pure (_A, c)
        Syntax.List elements -> do
            case elements of
                [] -> do
                    existential <- fresh

                    push (Context.UnsolvedType existential)
                    c <- cont
                    pure (Type.List{type_ = Type.UnsolvedType{..}, ..}, c)
                y : ys -> do
                    type_ <- fst <$> infer y cont

                    let process element = do
                            _Γ <- get

                            check element (Context.solveType _Γ type_)

                    traverse_ process ys
                    c <- cont
                    pure (Type.List{..}, c)
        Syntax.If predicate ifTrue ifFalse -> do
            check predicate Type.Scalar{scalar = Monotype.Bool, ..}

            (_L0, c) <- infer ifTrue cont

            _Γ <- get

            let _L1 = Context.solveType _Γ _L0

            check ifFalse _L1

            pure (_L1, c)

        -- All the type inference rules for scalars go here.  This part is
        -- pretty self-explanatory: a scalar literal pures the matching
        -- scalar type.
        Syntax.Float _ -> do
            (Type.Scalar{scalar = Monotype.Real, ..},) <$> cont
        Syntax.Int _ -> do
            (Type.Scalar{scalar = Monotype.Integer, ..},) <$> cont
        Syntax.String _ -> do
            (Type.Scalar{scalar = Monotype.Text, ..},) <$> cont
        Syntax.Char _ -> do
            (Type.Scalar{scalar = Monotype.Char, ..},) <$> cont
        Syntax.Unit -> do
            (Type.Scalar{scalar = Monotype.Unit, ..},) <$> cont
        Syntax.Block exprs -> error "TODO: block"
        Syntax.Match _ _ -> error "TODO: Implement match"
        Syntax.Tuple fields -> do
            types <- fst <<$>> traverse (`infer` cont) fields
            c <- cont
            pure (Type.Tuple{types, ..}, c)

{- | This corresponds to the judgment:

    > Γ ⊢ e ⇐ A ⊣ Δ

    … which checks that e has type A under input context Γ, producing an updated
    context Δ.
-}
check ::
    (HasCallStack) =>
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Expr ->
    Type SourceRegion ->
    Sem r ()
check expr t = Lens.traverseOf_ (_Expr . unlocated) (`check'` t) expr
  where
    -- The check function is the most important function to understand for the
    -- bidirectional type-checking algorithm.
    --
    -- Most people, when they first run across the `check` function think that you
    -- could get rid of most rules except for the final `Sub` rule, but that's not
    -- true!
    --
    -- The reason you should add `check` rules for many more types (especially
    -- complex types) is to ensure that subtyping rules work correctly.  For
    -- example, consider this expression:
    --
    --     [ 2, -3 ]
    --
    -- If you omit the `check` rule for `List`s then the above expression will
    -- fail to type-check because the first element of the list is a `Natural`
    -- number and the second element of the `List` is an `Integer`.
    --
    -- However, if you keep the `check` rule for `List`s and add a type annotation:
    --
    --     [ 2, -3 ] : List Integer
    --
    -- … then it works because the interpreter knows to treat both elements as an
    -- `Integer`.
    --
    -- In general, if you want subtyping to work reliably then you need to add
    -- more cases to the `check` function so that the interpreter can propagate
    -- top-level type annotations down to the "leaves" of your syntax tree.  If
    -- you do this consistently then the user only ever needs to provide top-level
    -- type annotations to fix any type errors that they might encounter, which is
    -- a desirable property!

    -- →I
    check' (Syntax.Lambda name body) Type.Function{..} = do
        scoped (Context.Annotation (mkLocal' name) input) do
            check body output

    -- ∃I
    check' e Type.Exists{domain = Domain.Type, ..} = do
        scopedUnsolvedType nameLocation \a -> do
            check' e (Type.substituteType name 0 a type_)
    check' e Type.Exists{domain = Domain.Fields, ..} = do
        scopedUnsolvedFields \a -> do
            check' e (Type.substituteFields name 0 a type_)
    check' e Type.Exists{domain = Domain.Alternatives, ..} = do
        scopedUnsolvedAlternatives \a -> do
            check' e (Type.substituteAlternatives name 0 a type_)

    -- ∀I
    check' e Type.Forall{..} = do
        scoped (Context.Variable domain name) do
            check' e type_
    check' (Syntax.List elements) Type.List{..} = do
        let process element = do
                _Γ <- get

                check element (Context.solveType _Γ type_)

        traverse_ process elements
    check' (Syntax.Tuple elements) Type.Tuple{types} = do
        let process (element, type_) = do
                _Γ <- get

                check element (Context.solveType _Γ type_)

        traverse_ process (NE.zip elements types)

    -- Sub
    check' e _B = do
        _A <- infer' expr

        _Θ <- get

        subtype (Context.solveType _Θ _A) (Context.solveType _Θ _B)

{- | This corresponds to the judgment:

    > Γ ⊢ A • e ⇒⇒ C ⊣ Δ

    … which infers the result type C when a function of type A is applied to an
    input argument e, under input context Γ, producing an updated context Δ.
-}
inferApplication ::
    (HasCallStack) =>
    (Member (State Status) r, Member (Error TypeInferenceError) r) =>
    Type SourceRegion ->
    Expr ->
    Sem r (Type SourceRegion)
-- ∀App
inferApplication Type.Forall{domain = Domain.Type, ..} e = do
    a <- fresh

    push (Context.UnsolvedType a)

    let a' = Type.UnsolvedType{location = nameLocation, existential = a}

    inferApplication (Type.substituteType name 0 a' type_) e
inferApplication Type.Forall{domain = Domain.Fields, ..} e = do
    a <- fresh

    push (Context.UnsolvedFields a)

    let a' = Type.Fields [] (Monotype.UnsolvedFields a)

    inferApplication (Type.substituteFields name 0 a' type_) e
inferApplication Type.Forall{domain = Domain.Alternatives, ..} e = do
    a <- fresh

    push (Context.UnsolvedAlternatives a)

    let a' = Type.Alternatives [] (Monotype.UnsolvedAlternatives a)

    inferApplication (Type.substituteAlternatives name 0 a' type_) e

-- ∃App
inferApplication Type.Exists{..} e = do
    scoped (Context.Variable domain name) do
        inferApplication type_ e

-- αApp
inferApplication Type.UnsolvedType{existential = a, ..} e = do
    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedType a _Γ `orDie` MissingVariable a _Γ

    a1 <- fresh
    a2 <- fresh

    set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

    check e Type.UnsolvedType{existential = a1, ..}

    pure Type.UnsolvedType{existential = a2, ..}
inferApplication Type.Function{..} e = do
    check e input

    pure output
inferApplication Type.VariableType{..} _ = do
    throw (NotNecessarilyFunctionType location name)
inferApplication _A _ = do
    throw (NotFunctionType (location _A) _A)

-- | Infer the `Type` of the given `Expr`
typeOf ::
    (Member (Error TypeInferenceError) r) =>
    Expr ->
    Sem r (Type SourceRegion)
typeOf = evalState [] . typeWith

-- | Like `typeOf`, but accepts a custom type-checking `Context`
typeWith ::
    ( Member (Error TypeInferenceError) r
    , Member (State (Context SourceRegion)) r
    ) =>
    Expr ->
    Sem r (Type SourceRegion)
typeWith syntax = do
    context' <- State.get
    let initialStatus = Status{count = 0, context = context'}

    (status, _A) <- runState initialStatus (infer' syntax)
    State.put (context status)

    pure _A

-- | Like `typeOf`, but accepts a custom type-checking `Context`
typeWithCont ::
    ( Member (Error TypeInferenceError) r
    , Member (State Status) r
    ) =>
    Expr ->
    Sem r a ->
    Sem r (Type SourceRegion, a)
typeWithCont syntax cont = do
    (_A, a) <- infer syntax cont

    pure (_A, a)

-- instance Exception TypeInferenceError where
--     displayException (IllFormedAlternatives location a0 _Γ) =
--         "Internal error: Invalid context\n\
--         \\n\
--         \The following unsolved alternatives variable:\n\
--         \\n\
--         \"
--             <> insert (Context.UnsolvedAlternatives a0)
--             <> "\n\
--                \\n\
--                \… is not well-formed within the following context:\n\
--                \\n\
--                \#{listToText _Γ}\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--     displayException (IllFormedFields location a0 _Γ) =
--         "Internal error: Invalid context\n\
--         \\n\
--         \The following unsolved fields variable:\n\
--         \\n\
--         \"
--             <> insert (Context.UnsolvedFields a0)
--             <> "\n\
--                \\n\
--                \… is not well-formed within the following context:\n\
--                \\n\
--                \"
--             <> listToText _Γ
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--     displayException (IllFormedType location _A _Γ) =
--         "Internal error: Invalid context\n\
--         \\n\
--         \The following type:\n\
--         \\n\
--         \"
--             <> insert _A
--             <> "\n\
--                \\n\
--                \… is not well-formed within the following context:\n\
--                \\n\
--                \"
--             <> listToText _Γ
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--     displayException (InvalidOperands location _L') =
--         "Invalid operands\n\
--         \\n\
--         \You cannot add values of type:\n\
--         \\n\
--         \"
--             <> insert _L'
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--     displayException (MergeConcreteRecord location _R) =
--         "Must merge a concrete record\n\
--         \\n\
--         \The first argument to a merge expression must be a record where all fields are\n\
--         \statically known.  However, you provided an argument of type:\n\
--         \\n\
--         \"
--             <> insert _R
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--             <> "\n\
--                \\n\
--                \… where not all fields could be inferred."
--     displayException (MergeInvalidHandler location _A) =
--         "Invalid handler\n\
--         \\n\
--         \The merge keyword expects a record of handlers where all handlers are functions,\n\
--         \but you provided a handler of the following type:\n\
--         \\n\
--         \"
--             <> insert _A
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--             <> "\n\
--                \\n\
--                \… which is not a function type."
--     displayException (MergeRecord location _R) =
--         "Must merge a record\n\
--         \\n\
--         \The first argument to a merge expression must be a record, but you provided an\n\
--         \expression of the following type:\n\
--         \\n\
--         \"
--             <> insert _R
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--             <> "\n\
--                \\n\
--                \… which is not a record type."
--     displayException (MissingAllAlternatives p0 _Γ) =
--         "Internal error: Invalid context\n\
--         \\n\
--         \The following unsolved alternatives variable:\n\
--         \\n\
--         \"
--             <> insert (Context.UnsolvedAlternatives p0)
--             <> "\n\
--                \\n\
--                \… cannot be instantiated because the alternatives variable is missing from the\n\
--                \context:\n\
--                \\n\
--                \"
--             <> listToText _Γ
--     displayException (MissingAllFields p0 _Γ) =
--         "Internal error: Invalid context\n\
--         \\n\
--         \The following unsolved fields variable:\n\
--         \\n\
--         \"
--             <> insert (Context.UnsolvedFields p0)
--             <> "\n\
--                \\n\
--                \… cannot be instantiated because the fields variable is missing from the\n\
--                \context:\n\
--                \\n\
--                \"
--             <> listToText _Γ
--     displayException (MissingOneOfAlternatives locations p0 p1 _Γ) =
--         "Internal error: Invalid context\n\
--         \\n\
--         \One of the following alternatives variables:\n\
--         \\n\
--         \"
--             <> listToText [Context.UnsolvedAlternatives p0, Context.UnsolvedAlternatives p1]
--             <> "\n\
--                \\n\
--                \… is missing from the following context:\n\
--                \\n\
--                \"
--             <> listToText _Γ
--             <> "\n\
--                \\n\
--                \"
--             <> locations'
--       where
--         locations' =
--             Text.unpack (Text.unlines (map (SourceRegion.renderError "") locations))
--     displayException (MissingOneOfFields locations p0 p1 _Γ) =
--         "Internal error: Invalid context\n\
--         \\n\
--         \One of the following fields variables:\\n\
--         \\n\
--         \"
--             <> listToText [Context.UnsolvedFields p0, Context.UnsolvedFields p1]
--             <> "\n\
--                \\n\
--                \… is missing from the following context:\n\
--                \\n\
--                \"
--             <> listToText _Γ
--             <> "\n\
--                \\n\
--                \"
--             <> locations'
--       where
--         locations' =
--             Text.unpack (Text.unlines (map (SourceRegion.renderError "") locations))
--     displayException (MissingVariable a _Γ) =
--         "Internal error: Invalid context\n\
--         \\n\
--         \The following unsolved variable:\n\
--         \\n\
--         \"
--             <> insert (Context.UnsolvedType a)
--             <> "\n\
--                \\n\
--                \… cannot be solved because the variable is missing from the context:\n\
--                \\n\
--                \"
--             <> listToText _Γ
--     displayException (NotFunctionType location _A) =
--         "Not a function type\n\
--         \\n\
--         \An expression of the following type:\n\
--         \\n\
--         \"
--             <> insert _A
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--             <> "\n\
--                \\n\
--                \… was invoked as if it were a function, but the above type is not a function\n\
--                \type."
--     displayException (NotNecessarilyFunctionType location a) =
--         "Not necessarily a function type\n\
--         \\n\
--         \The following type variable:\n\
--         \\n\
--         \"
--             <> insert a
--             <> "\n\
--                \\n\
--                \… could potentially be any type and is not necessarily a function type.\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--     displayException (NotAlternativesSubtype location p0 alternatives) =
--         "Not an alternatives subtype\n\
--         \\n\
--         \The following alternatives variable:\n\
--         \\n\
--         \"
--             <> insert p0
--             <> "\n\
--                \\n\
--                \… cannot be instantiated to the following union type:\n\
--                \\n\
--                \"
--             <> insert (Type.Union location alternatives)
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--             <> "\n\
--                \\n\
--                \… because the same alternatives variable appears within that union type."
--     displayException (NotFieldsSubtype location p0 fields) =
--         "Not a fields subtype\n\
--         \\n\
--         \The following fields variable:\n\
--         \\n\
--         \"
--             <> insert p0
--             <> "\n\
--                \\n\
--                \… cannot be instantiated to the following record type:\n\
--                \\n\
--                \"
--             <> insert (Type.Record location fields)
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--             <> "\n\
--                \\n\
--                \… because the same fields variable appears within that record type."
--     displayException (NotRecordSubtype locA0 _A locB0 _B) =
--         "Not a record subtype\n\
--         \\n\
--         \The following type:\n\
--         \\n\
--         \"
--             <> insert _A
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" locA0)
--             <> "\n\
--                \\n\
--                \… cannot be a subtype of:\n\
--                \\n\
--                \"
--             <> insert _B
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" locB0)
--     displayException (NotUnionSubtype locA0 _A locB0 _B) =
--         "Not a union subtype\n\
--         \\n\
--         \The following type:\n\
--         \\n\
--         \"
--             <> insert _A
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" locA0)
--             <> "\n\
--                \\n\
--                \… cannot be a subtype of:\n\
--                \\n\
--                \"
--             <> insert _B
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" locB0)
--     displayException (NotSubtype locA0 _A locB0 _B) =
--         "Not a subtype\n\
--         \\n\
--         \The following type:\n\
--         \\n\
--         \"
--             <> insert _A
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" locA0)
--             <> "\n\
--                \\n\
--                \… cannot be a subtype of:\n\
--                \\n\
--                \"
--             <> insert _B
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" locB0)
--     displayException (UnboundAlternatives location a) =
--         "Unbound alternatives variable: "
--             <> Text.unpack a
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--     displayException (UnboundFields location a) =
--         "Unbound fields variable: "
--             <> Text.unpack a
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--     displayException (UnboundTypeVariable location a) =
--         "Unbound type variable: "
--             <> Text.unpack a
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--     displayException (UnboundVariable location name index) =
--         "Unbound variable: "
--             <> Text.unpack var
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" location)
--       where
--         var = prettyToText @(Syntax.Syntax () Void) Syntax.Variable{location = (), ..}
--     displayException (RecordTypeMismatch _A0 _B0 extraA extraB)
--         | extraB == mempty =
--             "Record type mismatch\n\
--             \\n\
--             \The following record type:\n\
--             \\n\
--             \"
--                 <> insert _A0
--                 <> "\n\
--                    \\n\
--                    \"
--                 <> Text.unpack (SourceRegion.renderError "" (Type.location _A0))
--                 <> "\n\
--                    \\n\
--                    \… is not a subtype of the following record type:\n\
--                    \\n\
--                    \"
--                 <> insert _B0
--                 <> "\n\
--                    \\n\
--                    \"
--                 <> Text.unpack (SourceRegion.renderError "" (Type.location _B0))
--                 <> "\n\
--                    \\n\
--                    \The former record has the following extra fields:\n\
--                    \\n\
--                    \"
--                 <> listToText (Map.keys extraA)
--     displayException (RecordTypeMismatch _A0 _B0 extraA extraB)
--         | extraA == mempty =
--             "Record type mismatch\n\
--             \\n\
--             \The following record type:\n\
--             \\n\
--             \"
--                 <> insert _A0
--                 <> "\n\
--                    \\n\
--                    \"
--                 <> Text.unpack (SourceRegion.renderError "" (Type.location _A0))
--                 <> "\n\
--                    \\n\
--                    \… is not a subtype of the following record type:\n\
--                    \\n\
--                    \"
--                 <> insert _B0
--                 <> "\n\
--                    \\n\
--                    \"
--                 <> Text.unpack (SourceRegion.renderError "" (Type.location _B0))
--                 <> "\n\
--                    \\n\
--                    \The latter record has the following extra fields:\n\
--                    \\n\
--                    \"
--                 <> listToText (Map.keys extraB)
--     displayException (RecordTypeMismatch _A0 _B0 extraA extraB) =
--         "Record type mismatch\n\
--         \\n\
--         \The following record type:\n\
--         \\n\
--         \"
--             <> insert _A0
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" (Type.location _A0))
--             <> "\n\
--                \\n\
--                \… is not a subtype of the following record type:\n\
--                \\n\
--                \"
--             <> insert _B0
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" (Type.location _B0))
--             <> "\n\
--                \\n\
--                \The former record has the following extra fields:\n\
--                \\n\
--                \"
--             <> listToText (Map.keys extraA)
--             <> "\n\
--                \\n\
--                \… while the latter record has the following extra fields:\n\
--                \\n\
--                \"
--             <> listToText (Map.keys extraB)
--     displayException (UnionTypeMismatch _A0 _B0 extraA extraB)
--         | extraB == mempty =
--             "Union type mismatch\n\
--             \\n\
--             \The following union type:\n\
--             \\n\
--             \"
--                 <> insert _A0
--                 <> "\n\
--                    \\n\
--                    \"
--                 <> Text.unpack (SourceRegion.renderError "" (Type.location _A0))
--                 <> "\n\
--                    \\n\
--                    \… is not a subtype of the following union type:\n\
--                    \\n\
--                    \"
--                 <> insert _B0
--                 <> "\n\
--                    \\n\
--                    \"
--                 <> Text.unpack (SourceRegion.renderError "" (Type.location _B0))
--                 <> "\n\
--                    \\n\
--                    \The former union has the following extra alternatives:\n\
--                    \\n\
--                    \"
--                 <> listToText (Map.keys extraA)
--     displayException (UnionTypeMismatch _A0 _B0 extraA extraB)
--         | extraA == mempty =
--             "Union type mismatch\n\
--             \\n\
--             \The following union type:\n\
--             \\n\
--             \"
--                 <> insert _A0
--                 <> "\n\
--                    \\n\
--                    \"
--                 <> Text.unpack (SourceRegion.renderError "" (Type.location _A0))
--                 <> "\n\
--                    \\n\
--                    \… is not a subtype of the following union type:\n\
--                    \\n\
--                    \"
--                 <> insert _B0
--                 <> "\n\
--                    \\n\
--                    \"
--                 <> Text.unpack (SourceRegion.renderError "" (Type.location _B0))
--                 <> "\n\
--                    \\n\
--                    \The latter union has the following extra alternatives:\n\
--                    \\n\
--                    \"
--                 <> listToText (Map.keys extraB)
--     displayException (UnionTypeMismatch _A0 _B0 extraA extraB) =
--         "Union type mismatch\n\
--         \\n\
--         \The following union type:\n\
--         \\n\
--         \"
--             <> insert _A0
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" (Type.location _A0))
--             <> "\n\
--                \\n\
--                \… is not a subtype of the following union type:\n\
--                \\n\
--                \"
--             <> insert _B0
--             <> "\n\
--                \\n\
--                \"
--             <> Text.unpack (SourceRegion.renderError "" (Type.location _B0))
--             <> "\n\
--                \\n\
--                \The former union has the following extra alternatives:\n\
--                \\n\
--                \"
--             <> listToText (Map.keys extraA)
--             <> "\n\
--                \\n\
--                \… while the latter union has the following extra alternatives:\n\
--                \\n\
--                \"
--             <> listToText (Map.keys extraB)
