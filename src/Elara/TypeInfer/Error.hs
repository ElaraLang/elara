{-# LANGUAGE RecordWildCards #-}

-- | Type inference error types with rich context for error reporting.
module Elara.TypeInfer.Error (
    -- * Error Types
    UnifyError (..),
    UnifyErrorKind (..),

    -- * Error Construction
    mkUnifyError,
    mkUnifyErrorFromConstraint,
)
where

import Data.Map qualified as Map
import Error.Diagnose.Position qualified as Diag

import Elara.AST.Name (NameLike (..), Qualified, TypeName, VarName)
import Elara.AST.Region (SourceRegion, sourceRegionToDiagnosePosition)
import Elara.Data.AtLeast2List (AtLeast2List)
import Elara.Data.Pretty
import Elara.Data.Unique (Unique (..), uniqueId)
import Elara.Error (ElaraDiagnostic (..), ElaraMarker (..), ElaraMarkerType (..), ElaraNote (..), ElaraReport (..))
import Elara.TypeInfer.Context (ContextStack (..), InferenceContext (..), allContexts, currentContext, pushContext)
import Elara.TypeInfer.Render (renderMonotype)
import Elara.TypeInfer.Type (Constraint (..), DataCon, Monotype (..), Type, TypeVariable (..), constraintLoc, monotypeLoc)
import Elara.TypeInfer.Unique (UniqueTyVar)

import Elara.Error.Codes qualified as Codes

-- | The kind of unification error that occurred
data UnifyErrorKind
    = -- | Two types couldn't be unified
      TypeMismatch
    | -- | Two type constructors don't match
      TypeConstructorMismatch (Qualified TypeName) (Qualified TypeName)
    | -- | Occurs check failed (infinite type)
      OccursCheck TypeVariable
    | -- | Arity mismatch in type constructor application
      ArityMismatch Int Int
    | -- | Pattern constructor arity mismatch
      PatternArityMismatch DataCon Int Int
    | -- | Internal unification failed
      UnifyMismatch
    | -- | Polytypes can't be used as type aliases
      PolytypeAliasError
    deriving (Eq, Generic, Show)

-- | Enriched unification error with full context for error reporting
data UnifyError loc
    = UnifyError
        { ueKind :: !UnifyErrorKind
        -- ^ What kind of error occurred
        , ueExpected :: !(Monotype loc)
        -- ^ The expected type
        , ueActual :: !(Monotype loc)
        -- ^ The actual type found
        , ueExpectedUsage :: !loc
        -- ^ Where the expected type was inferred/used
        , ueActualUsage :: !loc
        -- ^ Where the actual type was found/provided
        , ueConstraintSite :: !loc
        -- ^ Where the constraint was generated
        , ueContext :: !(ContextStack (Monotype loc))
        -- ^ Why we were comparing these types
        }
    | -- | Unresolved constraint at the end of type checking
      UnresolvedConstraint (Qualified VarName) (Constraint loc)
    | -- | Polytypes can't be used as type aliases (legacy, keep for backwards compat)
      PolytypeAlias ([UniqueTyVar], Type loc)
    | MultipleUnifyErrors (AtLeast2List (UnifyError loc))
    deriving (Generic, Show)

-- | Create a UnifyError from a constraint and error kind
mkUnifyError ::
    UnifyErrorKind ->
    Monotype loc ->
    Monotype loc ->
    loc ->
    ContextStack (Monotype loc) ->
    UnifyError loc
mkUnifyError kind expected actual constraintSite ctx =
    UnifyError
        { ueKind = kind
        , ueExpected = expected
        , ueActual = actual
        , ueExpectedUsage = monotypeLoc expected
        , ueActualUsage = monotypeLoc actual
        , ueConstraintSite = constraintSite
        , ueContext = ctx
        }

-- | Create a UnifyError from a constraint, using its context information to fill in the error details.
mkUnifyErrorFromConstraint ::
    Eq loc =>
    UnifyErrorKind ->
    Monotype loc ->
    Monotype loc ->
    Constraint loc ->
    ContextStack (Monotype loc) ->
    UnifyError loc
mkUnifyErrorFromConstraint kind expected actual constraint ctx =
    let (expectedLoc, actualLoc) = case constraint of
            Equality{eqLeft, eqRight, eqLeftUsage, eqRightUsage, eqContext = constraintCtx} ->
                let eLoc
                        | expected `containsType` eqLeft = eqLeftUsage
                        | expected `containsType` eqRight = eqRightUsage
                        | otherwise = case constraintCtx of
                            Just (CheckingFunctionArgument{}) -> eqLeftUsage
                            _ -> monotypeLoc expected
                    aLoc
                        | actual `containsType` eqRight = eqRightUsage
                        | actual `containsType` eqLeft = eqLeftUsage
                        | otherwise = case constraintCtx of
                            Just (CheckingFunctionArgument{}) -> eqRightUsage
                            _ -> monotypeLoc actual
                 in (eLoc, aLoc)
            _ -> (monotypeLoc expected, monotypeLoc actual)
     in UnifyError
            { ueKind = kind
            , ueExpected = expected
            , ueActual = actual
            , ueExpectedUsage = expectedLoc
            , ueActualUsage = actualLoc
            , ueConstraintSite = constraintLoc constraint
            , ueContext = case constraint of
                Equality{eqContext = Just ctx'} -> pushContext ctx' ctx
                _ -> ctx
            }
  where
    containsType target source =
        target `eqModuloLoc` source || case source of
            TypeVar _ _ args -> any (containsType target) args
            TypeConstructor _ _ args -> any (containsType target) args
            Function _ a b -> containsType target a || containsType target b

-- | Check if two monotypes are equal, ignoring their source locations.
eqModuloLoc :: Monotype loc -> Monotype loc -> Bool
eqModuloLoc (TypeVar _ tv1 args1) (TypeVar _ tv2 args2) =
    tv1 == tv2 && length args1 == length args2 && and (zipWith eqModuloLoc args1 args2)
eqModuloLoc (TypeConstructor _ n1 args1) (TypeConstructor _ n2 args2) =
    (n1 == n2 || (nameText n1 == nameText n2)) && length args1 == length args2 && and (zipWith eqModuloLoc args1 args2)
eqModuloLoc (Function _ a1 b1) (Function _ a2 b2) =
    eqModuloLoc a1 a2 && eqModuloLoc b1 b2
eqModuloLoc _ _ = False

instance Pretty loc => Pretty (UnifyError loc)

instance Pretty UnifyErrorKind where
    pretty = \case
        TypeMismatch -> "Type mismatch"
        TypeConstructorMismatch a b -> "Type constructor mismatch:" <+> pretty a <+> "vs" <+> pretty b
        OccursCheck tv -> "Occurs check failed for" <+> pretty tv
        ArityMismatch expected actual -> "Arity mismatch: expected" <+> pretty expected <> ", got" <+> pretty actual
        PatternArityMismatch con expected actual -> "Pattern" <+> pretty con <+> "expects" <+> pretty expected <+> "arguments, got" <+> pretty actual
        UnifyMismatch -> "Unification failed"
        PolytypeAliasError -> "Polytype aliases are not supported"

-- | Build the main message for a unification error
buildMainMessage ::
    (Pretty loc, Typeable loc) =>
    Map UniqueTyVar Text ->
    UnifyError loc ->
    Doc AnsiStyle
buildMainMessage nameMap err = case err of
    UnifyError{..} ->
        let renamedContext = renameContext nameMap ueContext
         in case currentContext renamedContext of
                Just (CheckingFunctionArgument pos mFn fnType actualArgType _) ->
                    let expectedArgType =
                            let go 1 (Function _ d _) = d
                                go n (Function _ _ c) = go (n - 1) c
                                go _ t = t
                             in go pos fnType
                     in "Type mismatch"
                            <> maybe mempty (\fn -> " in call to" <+> squotes (pretty fn)) mFn
                            <> ":"
                                <+> renderMonotype expectedArgType
                                <+> "is not compatible with"
                                <+> renderMonotype actualArgType
                Just (CheckingIfCondition _) ->
                    "If condition must be Bool"
                Just (CheckingIfBranches _ _) ->
                    "If branches must have the same type"
                Just (CheckingMatchBranch idx _) ->
                    "Match branch" <+> pretty idx <+> "has incompatible type"
                Just (CheckingLetBinding name _) ->
                    "Type mismatch in binding" <+> squotes (pretty name)
                Just (CheckingAnnotation _) ->
                    "Expression doesn't match type annotation"
                _ ->
                    let expected' = renameVariables nameMap ueExpected
                        actual' = renameVariables nameMap ueActual
                     in case ueKind of
                            TypeConstructorMismatch _ _ -> "Type constructor mismatch"
                            OccursCheck _ -> "Infinite type detected"
                            ArityMismatch _ _ -> "Wrong number of type arguments"
                            PatternArityMismatch con _ _ -> "Wrong number of pattern arguments for" <+> pretty con
                            _ -> "Type mismatch:" <+> renderMonotype actual' <+> "vs" <+> renderMonotype expected'
    UnresolvedConstraint name constraint -> "Unresolved constraint in" <+> pretty name <> ":" <+> pretty constraint
    PolytypeAlias _ -> "Polytypes cannot be used as type aliases"
    MultipleUnifyErrors errors ->
        "Multiple unification errors occurred: "
            <> hsep (fmap (buildMainMessage nameMap) (toList errors))

instance (Show loc, Pretty loc, Typeable loc) => Exception (UnifyError loc)

instance ElaraDiagnostic (UnifyError SourceRegion) where
    diagnosticMessage err@UnifyError{..} =
        let allVars = ordNub (collectVars ueExpected <> collectVars ueActual <> collectContextVars ueContext)
            nameMap = buildNiceNameMap allVars
         in buildMainMessage nameMap err
    diagnosticMessage (UnresolvedConstraint name constraint) = "Unresolved constraint in" <+> pretty name <> ":" <+> pretty constraint
    diagnosticMessage (PolytypeAlias _) = "Polytypes cannot be used as type aliases"
    diagnosticMessage (MultipleUnifyErrors errors) =
        "Multiple unification errors occurred: "
            <> hsep (fmap diagnosticMessage (toList errors))

    diagnosticCode UnifyError{..} = case ueKind of
        TypeMismatch -> Just Codes.typeMismatch
        TypeConstructorMismatch{} -> Just Codes.typeConstructorMismatch
        OccursCheck{} -> Just Codes.occursCheckFailed
        PatternArityMismatch{} -> Just Codes.patternArityMismatch
        _ -> Nothing
    diagnosticCode _ = Nothing

    diagnosticMarkers UnifyError{..} =
        do
            let allVars = ordNub (collectVars ueExpected <> collectVars ueActual <> collectContextVars ueContext)
                nameMap = buildNiceNameMap allVars
                renamedContext = renameContext nameMap ueContext
            case ueKind of
                PatternArityMismatch con expectedCount actualCount ->
                    [ ElaraMarker
                        ueConstraintSite
                        PrimaryMarker
                        ("Pattern for constructor" <+> pretty con <+> "expects" <+> pretty expectedCount <+> "arguments, but got" <+> pretty actualCount)
                    ]
                _ -> case currentContext renamedContext of
                    Just (CheckingFunctionArgument pos _ fnType actualArgType _) ->
                        let expectedArgType =
                                let go 1 (Function _ d _) = d
                                    go n (Function _ _ c) = go (n - 1) c
                                    go _ t = t
                                 in go pos fnType
                            specificExpected = renameVariables nameMap ueExpected
                            specificActual = renameVariables nameMap ueActual
                            specificLoc = monotypeLoc ueActual
                            markers =
                                [ ElaraMarker ueExpectedUsage SecondaryMarker ("expected argument" <+> pretty pos <+> "to have type" <+> renderMonotype expectedArgType)
                                , ElaraMarker ueActualUsage SecondaryMarker ("but it has type" <+> renderMonotype actualArgType)
                                ]
                            thirdMarker =
                                ( [ ElaraMarker specificLoc PrimaryMarker ("expected" <+> renderMonotype specificExpected <+> "but found" <+> renderMonotype specificActual)
                                  | not (specificLoc == ueActualUsage && expectedArgType `eqModuloLoc` specificExpected && actualArgType `eqModuloLoc` specificActual)
                                  ]
                                )
                         in markers <> thirdMarker
                    _ ->
                        let expected' = renameVariables nameMap ueExpected
                            actual' = renameVariables nameMap ueActual
                            isSameLineOrOverlaps r1 r2 =
                                let p1 = sourceRegionToDiagnosePosition r1
                                    p2 = sourceRegionToDiagnosePosition r2
                                 in fst (Diag.begin p1) == fst (Diag.begin p2) && Diag.file p1 == Diag.file p2
                            (expectedMarkerType, actualMarkerType)
                                | ueExpectedUsage == ueConstraintSite = (PrimaryMarker, SecondaryMarker)
                                | ueActualUsage == ueConstraintSite = (SecondaryMarker, PrimaryMarker)
                                | ueExpectedUsage `isSameLineOrOverlaps` ueConstraintSite = (PrimaryMarker, SecondaryMarker)
                                | otherwise = (SecondaryMarker, PrimaryMarker)
                         in if expectedMarkerType == PrimaryMarker
                                then
                                    [ ElaraMarker ueExpectedUsage expectedMarkerType ("expected" <+> renderMonotype expected')
                                    , ElaraMarker ueActualUsage actualMarkerType ("but found" <+> renderMonotype actual')
                                    ]
                                else
                                    [ ElaraMarker ueActualUsage actualMarkerType ("but found" <+> renderMonotype actual')
                                    , ElaraMarker ueExpectedUsage expectedMarkerType ("expected" <+> renderMonotype expected')
                                    ]
    diagnosticMarkers (UnresolvedConstraint _ constraint) = [ElaraMarker (constraintLoc constraint) PrimaryMarker (pretty constraint)]
    diagnosticMarkers (PolytypeAlias _) = []
    diagnosticMarkers (MultipleUnifyErrors errors) = concatMap diagnosticMarkers (toList errors)

    diagnosticNotes UnifyError{..} = do
        let allVars = ordNub (collectVars ueExpected <> collectVars ueActual <> collectContextVars ueContext)
            nameMap = buildNiceNameMap allVars
            expected' = renameVariables nameMap ueExpected
            actual' = renameVariables nameMap ueActual
            renamedContext = renameContext nameMap ueContext

            contextNotes =
                mapMaybe
                    ( \case
                        CheckingFunctionArgument{} -> Nothing
                        c -> Just (Elara.Error.Note (pretty c))
                    )
                    (allContexts renamedContext)
            fnTypeNote = case currentContext renamedContext of
                Just (CheckingFunctionArgument _ (Just fnName) fnType _ _) ->
                    let label = case fnType of
                            Function{} -> "The function"
                            _ -> "The value"
                     in [Elara.Error.Note (label <+> squotes (pretty fnName) <+> "has type:" <+> renderMonotype fnType)]
                _ -> []
            nonFunctionHint = case (expected', actual') of
                (Function{}, TypeConstructor _ name _) ->
                    [Elara.Error.Hint ("Type" <+> pretty name <+> "is not a function and cannot be applied to arguments")]
                _ -> []
            ctorTypeNote = case ueKind of
                PatternArityMismatch con _ _ ->
                    [Elara.Error.Note ("The constructor" <+> squotes (pretty con) <+> "has type:" <+> renderMonotype expected')]
                _ -> []
            baseHint = case currentContext ueContext of
                Just (CheckingIfCondition _) ->
                    [Elara.Error.Hint "The condition of an 'if' expression must have type Bool"]
                Just (CheckingIfBranches _ _) ->
                    [Elara.Error.Hint "Both branches of an 'if' must return the same type"]
                _ -> []
         in contextNotes <> fnTypeNote <> ctorTypeNote <> baseHint <> nonFunctionHint
    diagnosticNotes (MultipleUnifyErrors errors) =
        concat $
            zipWith
                ( \i err ->
                    Elara.Error.Note (pretty i <> ".") : diagnosticNotes err
                )
                ([1 ..] :: [Int])
                (toList errors)
    diagnosticNotes _ = []

    diagnosticReports (MultipleUnifyErrors errors) =
        concatMap diagnosticReports (toList errors)
    diagnosticReports e =
        [ ElaraReport
            (diagnosticSeverity e)
            (diagnosticCode e)
            (diagnosticMessage e)
            (diagnosticMarkers e)
            (diagnosticNotes e)
        ]

-- | collect all the type variables that appear in a 'Monotype'
collectVars :: Monotype loc -> [UniqueTyVar]
collectVars (TypeVar _ tv args) =
    let v = case tv of
            UnificationVar u -> u
            SkolemVar u -> u
     in v : concatMap collectVars args
collectVars (TypeConstructor _ _ args) = concatMap collectVars args
collectVars (Function _ t1 t2) = collectVars t1 <> collectVars t2

-- | collect all the type variables that appear in a 'ContextStack'
collectContextVars :: ContextStack (Monotype loc) -> [UniqueTyVar]
collectContextVars (ContextStack stack) = concatMap collectContextVars' stack
  where
    collectContextVars' = \case
        CheckingFunctionArgument _ _ t a _ -> collectVars t <> collectVars a
        _ -> []

-- | Build a mapping from type variables to nice names (a, b, c, ..., t0, t1, ...).
buildNiceNameMap :: [UniqueTyVar] -> Map UniqueTyVar Text
buildNiceNameMap tvs =
    let nameless = ordNub [tv | tv@(Unique Nothing _) <- tvs]
        nameList = [one c | c <- ['a' .. 'z']] <> ["t" <> show n | n <- [0 :: Int ..]]
     in Map.fromList (zip nameless nameList)

-- | Rename type variables in a monotype to have nice names for error messages
renameVariables :: Map UniqueTyVar Text -> Monotype loc -> Monotype loc
renameVariables m = \case
    TypeVar loc tv args ->
        let renameUnique u =
                case Map.lookup u m of
                    Just name -> Unique (Just name) (u ^. uniqueId)
                    Nothing -> u
            tv' = case tv of
                UnificationVar u -> UnificationVar (renameUnique u)
                SkolemVar u -> SkolemVar (renameUnique u)
         in TypeVar loc tv' (renameVariables m <$> args)
    TypeConstructor loc dc args ->
        TypeConstructor loc dc (renameVariables m <$> args)
    Function loc t1 t2 ->
        Function loc (renameVariables m t1) (renameVariables m t2)

renameContext :: Map UniqueTyVar Text -> ContextStack (Monotype loc) -> ContextStack (Monotype loc)
renameContext m (ContextStack stack) = ContextStack (fmap renameContext' stack)
  where
    renameContext' :: InferenceContext (Monotype loc) -> InferenceContext (Monotype loc)
    renameContext' = \case
        CheckingFunctionArgument pos fnName t a cs ->
            CheckingFunctionArgument pos fnName (renameVariables m t) (renameVariables m a) cs
        other -> other
