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

import Elara.AST.Name (Qualified, TypeName, VarName)
import Elara.AST.Region (SourceRegion)
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
    UnifyErrorKind ->
    Monotype loc ->
    Monotype loc ->
    Constraint loc ->
    ContextStack (Monotype loc) ->
    UnifyError loc
mkUnifyErrorFromConstraint kind expected actual constraint ctx =
    UnifyError
        { ueKind = kind
        , ueExpected = actual -- swap the two
        , ueActual = expected
        , ueExpectedUsage = case constraint of
            Equality{eqRightUsage} -> eqRightUsage
            _ -> monotypeLoc expected
        , ueActualUsage = case constraint of
            Equality{eqLeftUsage} -> eqLeftUsage
            _ -> monotypeLoc actual
        , ueConstraintSite = constraintLoc constraint
        , ueContext = case constraint of
            Equality{eqContext = Just ctx'} -> pushContext ctx' ctx
            _ -> ctx
        }

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
buildMainMessage :: ContextStack (Monotype loc) -> UnifyErrorKind -> Doc AnsiStyle
buildMainMessage ctx kind = case currentContext ctx of
    Just (CheckingFunctionArgument _ mFn t _) ->
        "Type mismatch" <> maybe mempty (\fn -> " in call to" <+> squotes (pretty fn)) mFn <> " with type" <+> renderMonotype t
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
    _ -> case kind of
        TypeConstructorMismatch _ _ -> "Type constructor mismatch"
        OccursCheck _ -> "Infinite type detected"
        ArityMismatch _ _ -> "Wrong number of type arguments"
        PatternArityMismatch con _ _ -> "Wrong number of pattern arguments for" <+> pretty con
        _ -> "Type mismatch"

instance (Show loc, Pretty loc, Typeable loc) => Exception (UnifyError loc)

instance ElaraDiagnostic (UnifyError SourceRegion) where
    diagnosticMessage UnifyError{..} = buildMainMessage ueContext ueKind
    diagnosticMessage (UnresolvedConstraint name constraint) = "Unresolved constraint in" <+> pretty name <> ":" <+> pretty constraint
    diagnosticMessage (PolytypeAlias _) = "Polytypes cannot be used as type aliases"
    diagnosticMessage (MultipleUnifyErrors errors) =
        "Multiple unification errors occurred: "
            <> hsep (fmap diagnosticMessage (toList errors))

    diagnosticCode UnifyError{..} = case ueKind of
        TypeMismatch -> Just Codes.typeMismatch
        TypeConstructorMismatch{} -> Just Codes.typeConstructorMismatch
        OccursCheck{} -> Just Codes.occursCheckFailed
        _ -> Nothing
    diagnosticCode _ = Nothing

    diagnosticMarkers (UnifyError{..}) =
        do
            let allVars = ordNub (collectVars ueExpected <> collectVars ueActual)
                nameMap = buildNiceNameMap allVars
                expected' = renameVariables nameMap ueExpected
                actual' = renameVariables nameMap ueActual
            [ ElaraMarker ueExpectedUsage SecondaryMarker ("expected" <+> renderMonotype expected')
                , ElaraMarker ueActualUsage PrimaryMarker ("but found" <+> renderMonotype actual')
                ]
    diagnosticMarkers (UnresolvedConstraint _ constraint) = [ElaraMarker (constraintLoc constraint) PrimaryMarker (pretty constraint)]
    diagnosticMarkers (PolytypeAlias _) = []
    diagnosticMarkers (MultipleUnifyErrors errors) = concatMap diagnosticMarkers (toList errors)

    diagnosticNotes (UnifyError{..}) = do
        let allVars = ordNub (collectVars ueExpected <> collectVars ueActual)
            nameMap = buildNiceNameMap allVars
            expected' = renameVariables nameMap ueExpected
            actual' = renameVariables nameMap ueActual
            renamedContext = renameContext nameMap ueContext

            contextNotes = fmap (Elara.Error.Note . pretty) (allContexts renamedContext)
            unifyNote = Elara.Error.Note ("while unifying" <+> renderMonotype expected' <+> "with" <+> renderMonotype actual')
            typeNotes =
                [ Elara.Error.Note ("expected type:" <+> renderMonotype expected')
                , Elara.Error.Note ("actual type:  " <+> renderMonotype actual')
                ]
            fnTypeNote = case currentContext renamedContext of
                Just (CheckingFunctionArgument _ (Just fnName) fnType _) ->
                    let label = case fnType of
                            Function{} -> "The function"
                            _ -> "The value"
                     in [Elara.Error.Note (label <+> squotes (pretty fnName) <+> "has type:" <+> renderMonotype fnType)]
                _ -> []
            nonFunctionHint = case (expected', actual') of
                (Function{}, TypeConstructor _ name _) ->
                    [Elara.Error.Hint ("Type" <+> pretty name <+> "is not a function and cannot be applied to arguments")]
                _ -> []
            baseHint = case currentContext ueContext of
                Just (CheckingIfCondition _) ->
                    [Elara.Error.Hint "The condition of an 'if' expression must have type Bool"]
                Just (CheckingIfBranches _ _) ->
                    [Elara.Error.Hint "Both branches of an 'if' must return the same type"]
                _ -> []
         in contextNotes <> fnTypeNote <> [unifyNote] <> typeNotes <> baseHint <> nonFunctionHint
    diagnosticNotes (MultipleUnifyErrors errors) =
        concat $
            zipWith
                ( \i err ->
                    Elara.Error.Note (pretty i <> ".") : diagnosticNotes err
                )
                [1 ..]
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

renameContext :: Map UniqueTyVar Text -> ContextStack (Monotype SourceRegion) -> ContextStack (Monotype SourceRegion)
renameContext m (ContextStack stack) = ContextStack (fmap renameContext' stack)
  where
    renameContext' :: InferenceContext (Monotype SourceRegion) -> InferenceContext (Monotype SourceRegion)
    renameContext' = \case
        CheckingFunctionArgument pos fnName t cs ->
            CheckingFunctionArgument pos fnName (renameVariables m t) cs
        other -> other
