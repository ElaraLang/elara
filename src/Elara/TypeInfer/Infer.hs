{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

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

import Control.Lens ((^.), _1, _2)
import Data.Generics.Wrapped
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Traversable (for)
import Elara.AST.Generic (Expr (..), Expr' (..), Pattern (..))
import Elara.AST.Generic qualified as Syntax
import Elara.AST.Generic.Common
import Elara.AST.Name
import Elara.AST.Region (Located (..), SourceRegion (..), sourceRegion, unlocated)
import Elara.AST.Shunted
import Elara.AST.Typed
import Elara.AST.VarRef (mkLocal', withName')
import Elara.Data.Pretty (Pretty (..), prettyToText)
import Elara.Data.Unique
import Elara.Prim (primRegion, primitiveTCContext)
import Elara.TypeInfer.Context (Context, Entry)
import Elara.TypeInfer.Context qualified as Context
import Elara.TypeInfer.Domain qualified as Domain
import Elara.TypeInfer.Error
import Elara.TypeInfer.Existential (Existential (UnsafeExistential))
import Elara.TypeInfer.Monotype (Monotype)
import Elara.TypeInfer.Monotype qualified as Monotype
import Elara.TypeInfer.Type (Type (..))
import Elara.TypeInfer.Type qualified as Type
import Elara.TypeInfer.Unique
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.State (State)
import Polysemy.State qualified as State
import Prettyprinter qualified as Pretty
import Print (debugPretty, showPretty)
import TODO

-- | Type-checking state
data Status = Status
    { context :: Context SourceRegion
    -- ^ The type-checking context (e.g. Γ, Δ, Θ)
    , writeOnlyContext :: Context SourceRegion
    -- ^ A write-only context that logs every entry that is added to the main context
    }

initialStatus :: Member UniqueGen r => Sem r Status
initialStatus = do
    primitiveTCContext' <- primitiveTCContext
    pure
        Status
            { context = primitiveTCContext'
            , writeOnlyContext = primitiveTCContext'
            }

orDie :: Member (Error e) r => Maybe a -> e -> Sem r a
Just x `orDie` _ = pure x
Nothing `orDie` e = throw e

-- | Generate a fresh existential variable (of any type)
fresh :: Member UniqueGen r => Sem r (Existential a)
fresh = UnsafeExistential <$> makeUniqueTyVar

-- Unlike the original paper, we don't explicitly thread the `Context` around.
-- Instead, we modify the ambient state using the following utility functions:

-- | Push a new `Context` `Entry` onto the stack
push :: Member (State Status) r => Entry SourceRegion -> Sem r ()
push entry = State.modify (\s -> s{context = entry : context s, writeOnlyContext = entry : writeOnlyContext s})

-- | Retrieve the current `Context`
get :: Member (State Status) r => Sem r (Context SourceRegion)
get = State.gets context

getAll :: Member (State Status) r => Sem r (Context SourceRegion)
getAll = State.gets writeOnlyContext

-- | Set the `Context` to a new value
set :: Member (State Status) r => Context SourceRegion -> Sem r ()
set context = State.modify (\s -> s{context, writeOnlyContext = context <> writeOnlyContext s})

{- | This is used to temporarily add a `Context` entry that is discarded at the
 end of the entry's scope, along with any downstream entries that were
 created within that same scope
-}
scoped :: Member (State Status) r => Entry SourceRegion -> Sem r a -> Sem r a
scoped entry k = do
    push entry

    r <- k

    State.modify (\s -> s{context = Context.discardUpTo entry (context s)})

    pure r

scopedMany :: Member (State Status) r => NonEmpty (Entry SourceRegion) -> Sem r a -> Sem r a
scopedMany entries k = do
    traverse_ push entries

    r <- k

    State.modify (\s -> s{context = Context.discardUpTo (head entries) (context s)})

    pure r

scoped' :: Member (State Status) r => Sem r a -> Sem r a
scoped' k = do
    cur <- State.get
    r <- k

    State.put cur

    pure r

listening :: Member (State Status) r => Sem r a -> Sem r (a, Context SourceRegion)
listening k = do
    cur <- State.get
    r <- k
    cur' <- State.get

    pure
        ( r
        , case cur.context of
            [] -> []
            (x : xs) -> Context.discardUpToExcluding (head (x :| xs)) cur'.context
        )

scopedListening :: Member (State Status) r => Sem r a -> Sem r (a, Context SourceRegion)
scopedListening k = do
    cur <- State.get
    r <- k

    cur' <- State.get

    State.put cur

    pure
        ( r
        , case cur.context of
            [] -> []
            (x : xs) -> Context.discardUpToExcluding (head (x :| xs)) cur'.context
        )

scopedUnsolvedType :: (Member (State Status) r, Member UniqueGen r) => s -> (Type.Type s -> Sem r a) -> Sem r a
scopedUnsolvedType location k = do
    existential <- fresh

    scoped (Context.MarkerType existential) do
        push (Context.UnsolvedType existential)

        k Type.UnsolvedType{..}

scopedUnsolvedFields :: (Member (State Status) r, Member UniqueGen r) => (Type.Record s -> Sem r a) -> Sem r a
scopedUnsolvedFields k = do
    a <- fresh

    scoped (Context.MarkerFields a) do
        push (Context.UnsolvedFields a)

        k (Type.Fields [] (Monotype.UnsolvedFields a))

scopedUnsolvedAlternatives ::
    (Member (State Status) r, Member UniqueGen r) => (Type.Union s -> Sem r a) -> Sem r a
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
    HasCallStack =>
    Member (Error TypeInferenceError) r =>
    Context SourceRegion ->
    Type SourceRegion ->
    Sem r ()
wellFormedType _Γ type0 = case type0 of
    -- UvarWF
    Type.VariableType{..}
        | Context.Variable Domain.Type name `elem` _Γ -> pass
        | otherwise -> throw (UnboundTypeVariable location name _Γ)
    -- ArrowWF
    Type.Function{..} -> do
        wellFormedType _Γ input
        wellFormedType _Γ output

    -- ForallWF
    Type.Forall{..} -> wellFormedType (Context.Variable domain name : _Γ) type_
    -- ForallWF
    Type.Exists{..} -> wellFormedType (Context.Variable domain name : _Γ) type_
    -- EvarWF / SolvedEvarWF
    _A@Type.UnsolvedType{..}
        | any predicate _Γ -> pass
        | otherwise -> throw (IllFormedType location _A _Γ)
      where
        predicate (Context.UnsolvedType a) = existential == a
        predicate (Context.SolvedType a _) = existential == a
        predicate _ = False
    Type.Optional{..} -> wellFormedType _Γ type_
    Type.List{..} -> wellFormedType _Γ type_
    Type.Record{fields = Type.Fields kAs Monotype.EmptyFields} -> traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
    Type.Record{fields = Type.Fields kAs (Monotype.UnsolvedFields a0), ..}
        | any predicate _Γ -> traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
        | otherwise -> throw (IllFormedFields location a0 _Γ)
      where
        predicate (Context.UnsolvedFields a1) = a0 == a1
        predicate (Context.SolvedFields a1 _) = a0 == a1
        predicate _ = False
    Type.Record{fields = Type.Fields kAs (Monotype.VariableFields a), ..}
        | Context.Variable Domain.Fields a `elem` _Γ -> traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
        | otherwise -> throw (UnboundFields location a)
    Type.Union{alternatives = Type.Alternatives kAs Monotype.EmptyAlternatives} -> traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
    Type.Union{alternatives = Type.Alternatives kAs (Monotype.UnsolvedAlternatives a0), ..}
        | any predicate _Γ -> traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
        | otherwise -> throw (IllFormedAlternatives location a0 _Γ)
      where
        predicate (Context.UnsolvedAlternatives a1) = a0 == a1
        predicate (Context.SolvedAlternatives a1 _) = a0 == a1
        predicate _ = False
    Type.Union{alternatives = Type.Alternatives kAs (Monotype.VariableAlternatives a), ..}
        | Context.Variable Domain.Alternatives a `elem` _Γ -> traverse_ (\(_, _A) -> wellFormedType _Γ _A) kAs
        | otherwise -> throw (UnboundAlternatives location a)
    Type.Scalar{} -> pass
    Type.Tuple _ ts -> traverse_ (wellFormedType _Γ) ts
    Type.Custom _ _ ts -> traverse_ (wellFormedType _Γ) ts

{- | This corresponds to the judgment:

 > Γ ⊢ A <: B ⊣ Δ

 … which updates the context Γ to produce the new context Δ, given that the
 type A is a subtype of type B.
-}
subtype ::
    HasCallStack =>
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
    Type SourceRegion ->
    Type SourceRegion ->
    Sem r ()
subtype _A0 _B0 = do
    _Γ <- get

    case (_A0, _B0) of
        -- <:Var
        (Type.VariableType{name = a0}, Type.VariableType{name = a1})
            | a0 == a1 -> wellFormedType _Γ _A0
        -- <:Exvar
        (Type.UnsolvedType{existential = a0}, Type.UnsolvedType{existential = a1})
            | a0 == a1 && Context.UnsolvedType a0 `elem` _Γ -> pass
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
                && elem (Context.UnsolvedType a) _Γ ->
                instantiateTypeL a _B0
        -- InstantiateR
        (_, Type.UnsolvedType{existential = a})
            | not (a `Type.typeFreeIn` _A0)
                && elem (Context.UnsolvedType a) _Γ ->
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
        (_, Type.Exists{domain = Domain.Type, ..}) -> scopedUnsolvedType nameLocation \a ->
            subtype _A0 (Type.substituteType name a type_)
        (_, Type.Exists{domain = Domain.Fields, ..}) -> scopedUnsolvedFields \a -> subtype _A0 (Type.substituteFields name a type_)
        (_, Type.Exists{domain = Domain.Alternatives, ..}) -> scopedUnsolvedAlternatives \a -> subtype _A0 (Type.substituteAlternatives name a type_)
        -- <:∀L
        (Type.Forall{domain = Domain.Type, ..}, _) -> scopedUnsolvedType nameLocation \a -> subtype (Type.substituteType name a type_) _B0
        (Type.Forall{domain = Domain.Fields, ..}, _) -> scopedUnsolvedFields \a -> subtype (Type.substituteFields name a type_) _B0
        (Type.Forall{domain = Domain.Alternatives, ..}, _) -> scopedUnsolvedAlternatives \a -> subtype (Type.substituteAlternatives name a type_) _B0
        -- <:∃L
        (Type.Exists{..}, _) -> scoped (Context.Variable domain name) do
            subtype type_ _B0

        -- <:∀R
        (_, Type.Forall{..}) -> scoped (Context.Variable domain name) do
            subtype _A0 type_
        (Type.Scalar{scalar = s0}, Type.Scalar{scalar = s1})
            | s0 == s1 -> pass
        (Type.Optional{type_ = _A}, Type.Optional{type_ = _B}) -> subtype _A _B
        (Type.List{type_ = _A}, Type.List{type_ = _B}) -> subtype _A _B
        (Type.Tuple{tupleArguments = typesA}, Type.Tuple{tupleArguments = typesB}) -> do
            when (length typesA /= length typesB) do
                error "Tuple types must have the same number of elements"

            for_ (NE.zip typesA typesB) (uncurry subtype)
        -- This is where you need to add any non-trivial subtypes.  For example,
        -- the following three rules specify that `Natural` is a subtype of
        -- `Integer`, which is in turn a subtype of `Real`.
        (Type.Scalar{scalar = Monotype.Natural}, Type.Scalar{scalar = Monotype.Integer}) -> pass
        (Type.Scalar{scalar = Monotype.Natural}, Type.Scalar{scalar = Monotype.Real}) -> pass
        (Type.Scalar{scalar = Monotype.Integer}, Type.Scalar{scalar = Monotype.Real}) -> pass
        -- Similarly, this is the rule that says that `T` is a subtype of
        -- `Optional T`.  If that feels unprincipled to you then delete this
        -- rule.
        (_, Type.Optional{..}) -> subtype _A0 type_
        (Type.Scalar{}, Type.Scalar{scalar = Monotype.JSON}) -> pass
        (Type.List{type_ = _A}, Type.Scalar{scalar = Monotype.JSON}) -> do
            subtype _A _B0
            pass
        (Type.Record{fields = Type.Fields kAs Monotype.EmptyFields}, Type.Scalar{scalar = Monotype.JSON}) -> do
            let process (_, _A) = do
                    _Γ <- get

                    subtype _A (Context.solveType _Γ _B0)

            traverse_ process kAs

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
                        || flexible fields1 && fields0 /= fields1

            let okayB =
                    Map.null extraB
                        || flexible fields0 && fields0 /= fields1

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
                | otherwise -> pass

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
                _ | null extraA && null extraB && fields0 == fields1 -> pass
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

                            guard (Context.UnsolvedFields p1 `elem` _ΓR)

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

                            guard (Context.UnsolvedFields p0 `elem` _ΓR)

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
                        Nothing -> throw (MissingOneOfFields [Type.location _A0, Type.location _B0] p0 p1 _Γ)
                        Just setContext -> setContext

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
                (_, _) -> throw (NotRecordSubtype (Type.location _A0) _A (Type.location _B0) _B)

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
                        || flexible alternatives1 && alternatives0 /= alternatives1
            let okayB =
                    Map.null extraB
                        || flexible alternatives0 && alternatives0 /= alternatives1

            if
                | not okayA && not okayB -> do
                    throw (UnionTypeMismatch _A0 _B0 extraA extraB)
                | not okayA && okayB -> do
                    throw (UnionTypeMismatch _A0 _B0 extraA mempty)
                | okayA && not okayB -> do
                    throw (UnionTypeMismatch _A0 _B0 mempty extraB)
                | otherwise -> pass

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
                        Nothing -> throw (MissingOneOfAlternatives [Type.location _A0, Type.location _B0] p0 p1 _Γ)
                        Just setContext -> setContext

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
                (Monotype.EmptyAlternatives, Monotype.EmptyAlternatives) -> pass
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
                    | p0 == p1 -> pass
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
                (_, _) -> throw (NotUnionSubtype (Type.location _A0) _A (Type.location _B0) _B)
        (_A@Type.Custom{conName, typeArguments = kAs0}, _B@Type.Custom{conName = conName2, typeArguments = kBs0}) -> do
            when (conName /= conName2) do
                throw (CustomTypeMismatch _A0 _B0 conName conName2)

            let process (_A1, _B1) = do
                    _Θ <- get

                    subtype
                        (Context.solveType _Θ _A1)
                        (Context.solveType _Θ _B1)

            traverse_ process (zip kAs0 kBs0)

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
        (_A, _B) -> throw (NotSubtype (Type.location _A0) _A (Type.location _B0) _B)

{- | This corresponds to the judgment:

 > Γ ⊢ α̂ :≦ A ⊣ Δ

 … which updates the context Γ to produce the new context Δ, by instantiating
 α̂ such that α̂ <: A.

 The @instantiate*@ family of functions should really be called @solve*@
 because their job is to solve an unsolved variable within the context.
 However, for consistency with the paper we still name them @instantiate*@.
-}
instantiateTypeL ::
    (HasCallStack, Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
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
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType existential _Γ' ->
                set (_ΓR <> (Context.SolvedType existential (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _ΓL))
        -- InstLSolve
        Type.UnsolvedType{..} -> instLSolve (Monotype.UnsolvedType existential)
        Type.VariableType{..} -> instLSolve (Monotype.VariableType name)
        Type.Scalar{..} -> instLSolve (Monotype.Scalar scalar)
        -- InstLExt
        Type.Exists{domain = Domain.Type, ..} -> scopedUnsolvedType nameLocation \b -> instantiateTypeR (Type.substituteType name b type_) a
        Type.Exists{domain = Domain.Fields, ..} -> scopedUnsolvedFields \b -> instantiateTypeR (Type.substituteFields name b type_) a
        Type.Exists{domain = Domain.Alternatives, ..} -> scopedUnsolvedAlternatives \b -> instantiateTypeR (Type.substituteAlternatives name b type_) a
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
        Type.Forall{..} -> scoped (Context.Variable domain name) do
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
        Type.Custom{conName, typeArguments} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1s <- for typeArguments (const fresh)

            set (_ΓR <> (Context.SolvedType a (Monotype.Custom conName (Monotype.UnsolvedType <$> a1s)) : fmap Context.UnsolvedType a1s <> _ΓL))

            for_ (zip typeArguments a1s) \(typeArgument, a1) -> instantiateTypeL a1 typeArgument

{- | This corresponds to the judgment:

 > Γ ⊢ A ≦: α̂ ⊣ Δ

 … which updates the context Γ to produce the new context Δ, by instantiating
 α̂ such that A :< α̂.
-}
instantiateTypeR ::
    HasCallStack =>
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
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
            , Just (_ΓR, _ΓM) <- Context.splitOnUnsolvedType existential _Γ' ->
                set (_ΓR <> (Context.SolvedType existential (Monotype.UnsolvedType a) : _ΓM) <> (Context.UnsolvedType a : _ΓL))
        -- InstRSolve
        Type.UnsolvedType{..} -> instRSolve (Monotype.UnsolvedType existential)
        Type.VariableType{..} -> instRSolve (Monotype.VariableType name)
        Type.Scalar{..} -> instRSolve (Monotype.Scalar scalar)
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
        Type.Exists{..} -> scoped (Context.Variable domain name) do
            instantiateTypeL a type_

        -- InstRAllL
        Type.Forall{domain = Domain.Type, ..} -> scopedUnsolvedType nameLocation \b -> instantiateTypeR (Type.substituteType name b type_) a
        Type.Forall{domain = Domain.Fields, ..} -> scopedUnsolvedFields \b -> instantiateTypeR (Type.substituteFields name b type_) a
        Type.Forall{domain = Domain.Alternatives, ..} -> scopedUnsolvedAlternatives \b -> instantiateTypeR (Type.substituteAlternatives name b type_) a
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
        Type.Custom{..} -> do
            let _ΓL = _Γ
            let _ΓR = _Γ'

            a1s <- for typeArguments (const fresh)

            set (_ΓR <> (Context.SolvedType a (Monotype.Custom conName (Monotype.UnsolvedType <$> a1s)) : fmap Context.UnsolvedType a1s <> _ΓL))

            for_ (zip typeArguments a1s) (uncurry instantiateTypeR)

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

            guard (Context.UnsolvedFields p0 `elem` _ΓL)

            pure (set (_ΓR <> (Context.SolvedFields p1 (Monotype.Fields [] (Monotype.UnsolvedFields p0)) : _ΓL)))

    let p1First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedFields p0 _Γ0

            guard (Context.UnsolvedFields p1 `elem` _ΓL)

            pure (set (_ΓR <> (Context.SolvedFields p0 (Monotype.Fields [] (Monotype.UnsolvedFields p1)) : _ΓL)))

    case p0First <|> p1First of
        Nothing -> throw (MissingOneOfFields [] p0 p1 _Γ0)
        Just setContext -> setContext

instantiateFieldsL ::
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
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
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
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

            guard (Context.UnsolvedAlternatives p0 `elem` _ΓL)

            pure (set (_ΓR <> (Context.SolvedAlternatives p1 (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p0)) : _ΓL)))

    let p1First = do
            (_ΓR, _ΓL) <- Context.splitOnUnsolvedAlternatives p0 _Γ0

            guard (Context.UnsolvedAlternatives p1 `elem` _ΓL)

            pure (set (_ΓR <> (Context.SolvedAlternatives p0 (Monotype.Alternatives [] (Monotype.UnsolvedAlternatives p1)) : _ΓL)))

    case p0First <|> p1First of
        Nothing -> throw (MissingOneOfAlternatives [] p0 p1 _Γ0)
        Just setContext -> setContext

instantiateAlternativesL ::
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
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
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
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

{- | This corresponds to the judgment:

 > Γ ⊢ p ⇒ A ⊣ Δ

 … which infers the type of p under input context Γ, producing an inferred
 type of A and an updated context Δ.
-}
inferPattern ::
    (HasCallStack, Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
    ShuntedPattern ->
    Sem r TypedPattern
inferPattern (Syntax.Pattern (Located location e0, _)) = case e0 of
    Syntax.VarPattern v -> do
        a <- fresh
        push (Context.UnsolvedType a)
        push (Context.Annotation (mkLocal' $ NormalVarName <<$>> v) (Type.UnsolvedType{location, existential = a}))

        pure (Pattern (Located location (Syntax.VarPattern (NormalVarName <<$>> v)), Type.UnsolvedType location a))
    Syntax.WildcardPattern -> do
        a <- fresh

        pure (Pattern (Located location Syntax.WildcardPattern, Type.UnsolvedType location a))
    Syntax.IntegerPattern i -> pure (Pattern (Located location (Syntax.IntegerPattern i), Type.Scalar{scalar = Monotype.Integer, ..}))
    Syntax.FloatPattern f -> pure (Pattern (Located location (Syntax.FloatPattern f), Type.Scalar{scalar = Monotype.Real, ..}))
    Syntax.StringPattern s -> pure (Pattern (Located location (Syntax.StringPattern s), Type.Scalar{scalar = Monotype.Text, ..}))
    Syntax.CharPattern c -> pure (Pattern (Located location (Syntax.CharPattern c), Type.Scalar{scalar = Monotype.Char, ..}))
    Syntax.UnitPattern -> pure (Pattern (Located location Syntax.UnitPattern, Type.Scalar{scalar = Monotype.Unit, ..}))
    Syntax.ConstructorPattern{} -> todo
    Syntax.ListPattern ps -> scopedUnsolvedType location \a -> do
        let t = Type.List{location, type_ = a, ..}

        ps <- traverse inferPattern ps

        traverse_ ((`subtype` a) . Syntax.patternTypeOf) ps

        pure (Pattern (Located location (Syntax.ListPattern ps), t))
    Syntax.ConsPattern p0 p1 -> do
        a <- fresh

        let t = Type.List{location, type_ = Type.UnsolvedType{existential = a, ..}, ..}

        p0 <- inferPattern p0
        p1 <- inferPattern p1

        subtype (Syntax.patternTypeOf p0) (Type.UnsolvedType{existential = a, ..})
        subtype (Syntax.patternTypeOf p1) (Type.List location $ Type.UnsolvedType{existential = a, ..})

        _Θ <- get

        instantiateTypeL a (Context.solveType _Θ (Type.List location (Type.stripForAll t)))

        pure (Pattern (Located location (Syntax.ConsPattern p0 p1), t))

{- | This corresponds to the judgment:

 > Γ ⊢ e ⇒ A ⊣ Δ

 … which infers the type of e under input context Γ, producing an inferred
 type of A and an updated context Δ.
-}
infer ::
    HasCallStack =>
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
    ShuntedExpr ->
    Sem r TypedExpr
infer (Syntax.Expr (Located location e0, _)) = case e0 of
    -- Var
    Syntax.Var vn -> do
        _Γ <- get

        let n = withName' (vn ^. unlocated)

        t <- Context.lookup n _Γ `orDie` UnboundVariable (vn ^. sourceRegion) n _Γ

        pure $ Expr (Located location (Var vn), t)

    -- →I⇒
    Syntax.Lambda name body -> do
        a <- fresh
        b <- fresh

        let input = Type.UnsolvedType{location = name ^. sourceRegion, existential = a}

        let output = Type.UnsolvedType{existential = b, ..}

        push (Context.UnsolvedType a)
        push (Context.UnsolvedType b)

        body' <- scoped (Context.Annotation (mkLocal' name) input) do
            check body output

        let t = Type.Function{..}
        -- if input is unsolved, we need to create a type lambda, i.e. \(@a : Type) -> blah
        let actualLam = Syntax.Expr (Located location (Lambda name body'), t)
        case input of
            Type.UnsolvedType{} -> pure actualLam
            _ -> pure actualLam

    -- →E
    Syntax.FunctionCall function argument -> do
        _A <- infer function

        _Θ <- get

        (typedArgument, resultType) <- inferApplication (Context.solveType _Θ (_A ^. _Unwrapped . _2)) argument

        ctx <- get
        completedFunctionType <- Context.complete ctx (Type.stripForAll (Syntax.typeOf _A)) -- I don't like that this is necessary but we get redundant type applications otherwise
        let isFreeTypeVariable = \case Type.VariableType _ x -> not (Type.occurs x completedFunctionType); Type.UnsolvedType{} -> True; _ -> False
        e <- case Type.stripForAll completedFunctionType of
            Type.Function{input, output}
                | isFreeTypeVariable input && isFreeTypeVariable output -> do
                    pure $
                        FunctionCall
                            ( Expr
                                ( Located
                                    primRegion
                                    ( TypeApplication
                                        ( Expr
                                            ( Located
                                                primRegion
                                                (TypeApplication _A (Syntax.typeOf typedArgument))
                                            , resultType
                                            )
                                        )
                                        (Type.stripForAll resultType)
                                    )
                                , resultType
                                )
                            )
                            typedArgument
            Type.Function{input}
                | isFreeTypeVariable input -> do
                    pure $
                        FunctionCall
                            ( Expr (Located primRegion (TypeApplication _A (Syntax.typeOf typedArgument)), resultType)
                            )
                            typedArgument
            Type.Function{output}
                | isFreeTypeVariable output -> do
                    pure $
                        FunctionCall
                            ( Expr (Located primRegion (TypeApplication _A (Type.stripForAll resultType)), resultType)
                            )
                            typedArgument
            o -> do
                pure $ FunctionCall _A typedArgument

        pure $ Expr (Located location e, resultType)

    -- All the type inference rules for scalars go here.  This part is
    -- pretty self-explanatory: a scalar literal returns the matching
    -- scalar type.
    Syntax.Float f -> do
        let t = (Type.Scalar{scalar = Monotype.Real, ..})
        pure $ Expr (Located location (Float f), t)
    Syntax.Int i -> do
        let t = (Type.Scalar{scalar = Monotype.Integer, ..})
        pure $ Expr (Located location (Int i), t)
    Syntax.String s -> do
        let t = (Type.Scalar{scalar = Monotype.Text, ..})
        pure $ Expr (Located location (String s), t)
    Syntax.Unit -> do
        let t = (Type.Scalar{scalar = Monotype.Unit, ..})
        pure $ Expr (Located location Unit, t)
    Syntax.Char c -> do
        let t = (Type.Scalar{scalar = Monotype.Char, ..})
        pure $ Expr (Located location (Char c), t)
    Syntax.LetIn name NoFieldValue val body -> do
        -- TODO: infer whether a let-in is recursive or not
        -- insert a new unsolved type variable for the let-in to make recursive let-ins possible
        existential <- fresh

        push (Context.UnsolvedType existential)
        push (Context.Annotation (mkLocal' name) (Type.UnsolvedType primRegion existential))

        val'@(Expr (_, valType)) <-
            infer val

        ctx <- get

        valType' <- Context.complete ctx valType -- I have a feeling that this will break things
        -- Basically in a case of something like let id = \x -> x in id id, we have to 'complete' \x -> x early, otherwise we get a type error
        -- But this is probably not the right way to do it
        push (Context.Annotation (mkLocal' name) valType')

        body'@(Expr (_, bodyType)) <- infer body
        pure $ Expr (Located location (LetIn name NoFieldValue val' body'), bodyType)
    Syntax.Tuple elements -> do
        let process element = do
                _Γ <- get

                infer element

        elementTypes <- traverse process elements

        let t = (Type.Tuple{tupleArguments = Syntax.typeOf <$> elementTypes, ..})
        pure $ Expr (Located location (Syntax.Tuple elementTypes), t)
    Syntax.If cond then_ else_ -> do
        cond' <- check cond Type.Scalar{scalar = Monotype.Bool, ..}

        then' <- infer then_

        _Γ <- get

        let _L1 = Context.solveType _Γ (Syntax.typeOf then')

        else' <- check else_ _L1

        pure $ Expr (Located location (If cond' then' else'), _L1)
    Syntax.Match e branches -> do
        e' <- infer e

        returnType <- fresh -- create a monotype for the return of the match
        push (Context.UnsolvedType returnType)

        let process (pattern_, branch) = do
                pattern' <- checkPattern pattern_ (Syntax.typeOf e')

                branch' <-
                    -- check that the branch type matches the return type of the match
                    check branch (Type.UnsolvedType primRegion returnType)
                pure (pattern', branch')

        branches' <- traverse process branches

        pure $ Expr (Located location (Match e' branches'), Syntax.typeOf e')
    Syntax.List (y : ys) -> do
        y'@(Expr (_, type_)) <- infer y

        let process element = do
                _Γ <- get

                check element (Context.solveType _Γ type_)

        ys' <- traverse process ys

        pure $ Expr (Located location (Syntax.List (y' : ys')), Syntax.typeOf y')
    Syntax.Block exprs -> do
        let process expr = do
                _Γ <- get

                infer expr

        exprTypes <- traverse process exprs

        pure $ Expr (Located location (Syntax.Block exprTypes), Syntax.typeOf $ last exprTypes)
    other -> error $ "infer: " <> showPretty other

{- | This corresponds to the judgment:

 > Γ ⊢ e ⇐ A ⊣ Δ

 … which checks that e has type A under input context Γ, producing an updated
 context Δ.
-}
check ::
    forall r.
    HasCallStack =>
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
    ShuntedExpr ->
    Type SourceRegion ->
    Sem r TypedExpr
check expr@(Expr (Located exprLoc _, _)) t = do
    let x = expr ^. _Unwrapped . _1 . unlocated
    check' x t
  where
    check' :: ShuntedExpr' -> Type SourceRegion -> Sem r TypedExpr
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
    check' (Syntax.Lambda name body) Type.Function{..} = scoped (Context.Annotation (mkLocal' name) input) do
        o <- check body output
        pure $ Expr (Located exprLoc (Lambda name o), t)
    -- ∃I
    check' e Type.Exists{domain = Domain.Type, ..} = scopedUnsolvedType nameLocation \a -> check' e (Type.substituteType name a type_)
    check' e Type.Exists{domain = Domain.Fields, ..} = scopedUnsolvedFields \a -> check' e (Type.substituteFields name a type_)
    check' e Type.Exists{domain = Domain.Alternatives, ..} = scopedUnsolvedAlternatives \a -> check' e (Type.substituteAlternatives name a type_)
    -- ∀I
    check' e Type.Forall{..} = scoped (Context.Variable domain name) do
        check' e type_
    check' (Syntax.Tuple elements) Type.Tuple{tupleArguments} = do
        let process (element, type_) = do
                _Γ <- get

                check element (Context.solveType _Γ type_)

        y <- traverse process (NE.zip elements tupleArguments)

        pure $ Expr (Located exprLoc (Syntax.Tuple y), t)
    check' (Syntax.List elements) Type.List{..} = do
        let process element = do
                _Γ <- get

                check element (Context.solveType _Γ type_)

        y <- traverse process elements

        pure $ Expr (Located exprLoc (Syntax.List y), t)
    check' (Syntax.Match e branches) t = do
        e' <- infer e

        let process (pattern_, branch) = do
                pattern' <-
                    checkPattern pattern_ (Syntax.typeOf e')

                -- check that the branch type matches the return type of the match
                branch' <- check branch t

                pure (pattern', branch')

        branches' <- traverse process branches

        pure $ Expr (Located exprLoc (Match e' branches'), t)

    -- Sub
    check' _ _B = do
        _A@(Syntax.Expr (_, _At)) <- infer expr

        _Θ <- get

        subtype (Context.solveType _Θ _At) (Context.solveType _Θ _B)
        case _At of
            Type.Forall{} | Type.isMonoType t -> do
                -- insert type application from instantiating the forall
                debugPretty ("Inserted monotype" :: Text, _At, t)
                pure $ Expr (Located exprLoc (TypeApplication _A t), _At `Type.instantiate` t)
            _ -> pure _A

{- | This corresponds to the judgment:

 > Γ ⊢ p ⇐ A ⊣ Δ

 … which checks that p has type A under input context Γ, producing an updated
 context Δ.
-}
checkPattern ::
    forall r.
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
    ShuntedPattern ->
    Type SourceRegion ->
    Sem r TypedPattern
checkPattern pattern_@(Pattern (Located exprLoc _, _)) t = do
    let x = pattern_ ^. _Unwrapped . _1 . unlocated
    check' x t
  where
    check' :: ShuntedPattern' -> Type SourceRegion -> Sem r TypedPattern
    check' (Syntax.VarPattern vn) t = do
        push (Context.Annotation (mkLocal' $ NormalVarName <<$>> vn) t)
        pure $ Pattern (Located exprLoc (Syntax.VarPattern (NormalVarName <<$>> vn)), t) -- var patterns always match
    check' Syntax.WildcardPattern t = pure $ Pattern (Located exprLoc Syntax.WildcardPattern, t) -- wildcard patterns always match
    check' (Syntax.ConstructorPattern _ _) _ = todo
    check' (Syntax.ListPattern patterns) Type.List{..} = do
        let process element = do
                _Γ <- get

                checkPattern element (Context.solveType _Γ type_)

        y <- traverse process patterns

        pure $ Pattern (Located exprLoc (Syntax.ListPattern y), t)
    check' (Syntax.ConsPattern x xs) Type.List{..} = do
        x' <- checkPattern x type_
        xs' <- checkPattern xs (Type.List{..})

        pure $ Pattern (Located exprLoc (Syntax.ConsPattern x' xs'), t)
    check' (Syntax.IntegerPattern x) Type.Scalar{scalar = Monotype.Integer} = pure $ Pattern (Located exprLoc (Syntax.IntegerPattern x), t)
    check' (Syntax.FloatPattern x) Type.Scalar{scalar = Monotype.Real} = pure $ Pattern (Located exprLoc (Syntax.FloatPattern x), t)
    check' (Syntax.CharPattern x) Type.Scalar{scalar = Monotype.Char} = pure $ Pattern (Located exprLoc (Syntax.CharPattern x), t)
    check' (Syntax.StringPattern x) Type.Scalar{scalar = Monotype.Text} = pure $ Pattern (Located exprLoc (Syntax.StringPattern x), t)
    check' Syntax.UnitPattern Type.Scalar{scalar = Monotype.Unit} = pure $ Pattern (Located exprLoc Syntax.UnitPattern, t)
    -- Sub
    check' _ _B = do
        _A@(Syntax.Pattern (_, _At)) <- inferPattern pattern_

        _Θ <- get

        subtype (Context.solveType _Θ _At) (Context.solveType _Θ _B)
        pure _A

{- | This corresponds to the judgment:

 > Γ ⊢ A • e ⇒⇒ C ⊣ Δ

 … which infers the result type C when a function of type A is applied to an
 input argument e, under input context Γ, producing an updated context Δ.

 This has been adjusted to return the typed argument as well as C
-}
inferApplication ::
    HasCallStack =>
    (Member (State Status) r, Member (Error TypeInferenceError) r, Member UniqueGen r) =>
    Type SourceRegion ->
    ShuntedExpr ->
    Sem r (TypedExpr, Type SourceRegion)
-- ∀App
inferApplication Type.Forall{domain = Domain.Type, ..} e = do
    a <- fresh

    push (Context.UnsolvedType a)

    let a' = Type.UnsolvedType{location = nameLocation, existential = a}
    inferApplication (Type.substituteType name a' type_) e
inferApplication Type.Forall{domain = Domain.Fields, ..} e = do
    a <- fresh

    push (Context.UnsolvedFields a)

    let a' = Type.Fields [] (Monotype.UnsolvedFields a)

    inferApplication (Type.substituteFields name a' type_) e
inferApplication Type.Forall{domain = Domain.Alternatives, ..} e = do
    a <- fresh

    push (Context.UnsolvedAlternatives a)

    let a' = Type.Alternatives [] (Monotype.UnsolvedAlternatives a)

    inferApplication (Type.substituteAlternatives name a' type_) e

-- ∃App
inferApplication Type.Exists{..} e = scoped (Context.Variable domain name) do
    inferApplication type_ e

-- αApp
inferApplication Type.UnsolvedType{existential = a, ..} e = do
    _Γ <- get

    (_ΓR, _ΓL) <- Context.splitOnUnsolvedType a _Γ `orDie` MissingVariable a _Γ

    a1 <- fresh
    a2 <- fresh

    set (_ΓR <> (Context.SolvedType a (Monotype.Function (Monotype.UnsolvedType a1) (Monotype.UnsolvedType a2)) : Context.UnsolvedType a1 : Context.UnsolvedType a2 : _ΓL))

    e' <- check e Type.UnsolvedType{existential = a1, ..}

    let t = Type.UnsolvedType{existential = a2, ..}

    pure (e', t)
inferApplication Type.Function{..} e = do
    e' <- check e input

    pure (e', output)
inferApplication Type.VariableType{..} _ = throw (NotNecessarilyFunctionType location name)
inferApplication _A _ = throw (NotFunctionType (location _A) _A)

-- Helper functions for displaying errors

insert :: Pretty a => a -> String
insert a = toString (prettyToText ("  " <> Pretty.align (pretty a)))

listToText :: Pretty a => [a] -> String
listToText elements =
    toString (Text.intercalate "\n" (map prettyEntry elements))
  where
    prettyEntry entry = prettyToText ("• " <> Pretty.align (pretty entry))
