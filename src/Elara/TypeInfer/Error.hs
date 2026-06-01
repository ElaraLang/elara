{-# LANGUAGE RecordWildCards #-}

-- | Type inference error types with rich context for error reporting.
module Elara.TypeInfer.Error (
    -- * Error Types
    UnifyError (..),
    UnifyErrorKind (..),

    -- * Error Construction
    mkUnifyError,
    mkUnifyErrorFromConstraint,
) where

import Error.Diagnose hiding (Hint, Note)

import Elara.AST.Name (Qualified, TypeName, VarName)
import Elara.AST.Region (SourceRegion, sourceRegionToDiagnosePosition)
import Elara.Data.Pretty
import Elara.Error (ElaraDiagnostic (..), ElaraMarker (..), ElaraMarkerType (..), ElaraNote (..))
import Elara.Error.Diagnose (toDiagnoseReports)
import Elara.TypeInfer.Context (ContextStack (..), InferenceContext (..), allContexts, currentContext, pushContext)
import Elara.TypeInfer.Render (renderMonotype)
import Elara.TypeInfer.Type (Constraint (..), DataCon, Monotype (..), Type, TypeVariable, constraintLoc, monotypeLoc)
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
        , ueContext :: !ContextStack
        -- ^ Why we were comparing these types
        }
    | -- | Unresolved constraint at the end of type checking
      UnresolvedConstraint (Qualified VarName) (Constraint loc)
    | -- | Polytypes can't be used as type aliases (legacy, keep for backwards compat)
      PolytypeAlias ([UniqueTyVar], Type loc)
    deriving (Generic, Show)

-- | Create a UnifyError from a constraint and error kind
mkUnifyError ::
    UnifyErrorKind ->
    Monotype loc ->
    Monotype loc ->
    loc ->
    ContextStack ->
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

-- | Create a UnifyError from a constraint, using its context information
mkUnifyErrorFromConstraint ::
    UnifyErrorKind ->
    Monotype loc ->
    Monotype loc ->
    Constraint loc ->
    ContextStack ->
    UnifyError loc
mkUnifyErrorFromConstraint kind expected actual constraint ctx =
    UnifyError
        { ueKind = kind
        , ueExpected = expected
        , ueActual = actual
        , ueExpectedUsage = case constraint of
            Equality{eqLeftUsage} -> eqLeftUsage
            _ -> monotypeLoc expected
        , ueActualUsage = case constraint of
            Equality{eqRightUsage} -> eqRightUsage
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
buildMainMessage :: ContextStack -> UnifyErrorKind -> Doc AnsiStyle
buildMainMessage ctx kind = case currentContext ctx of
    Just (CheckingFunctionArgument _ mFn _) ->
        "Type mismatch" <> maybe mempty (\fn -> " in call to" <+> squotes (pretty fn)) mFn
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

-- | Relation that defines 2 types as equal iff they are equal under a substitution of type variables
instance (Show loc, Pretty loc, Typeable loc) => Exception (UnifyError loc)

instance ElaraDiagnostic (UnifyError SourceRegion) where
    diagnosticMessage UnifyError{..} = buildMainMessage ueContext ueKind
    diagnosticMessage (UnresolvedConstraint name constraint) = "Unresolved constraint in" <+> pretty name <> ":" <+> pretty constraint
    diagnosticMessage (PolytypeAlias _) = "Polytypes cannot be used as type aliases"

    diagnosticCode UnifyError{..} = case ueKind of
        TypeMismatch -> Just Codes.typeMismatch
        TypeConstructorMismatch{} -> Just Codes.typeConstructorMismatch
        OccursCheck{} -> Just Codes.occursCheckFailed
        _ -> Nothing
    diagnosticCode _ = Nothing

    diagnosticMarkers (UnifyError{..}) =
        let expectedUsage = ueExpectedUsage :: SourceRegion
            actualUsage = ueActualUsage :: SourceRegion
         in [ ElaraMarker expectedUsage SecondaryMarker ("expected" <+> renderMonotype ueExpected)
            , ElaraMarker actualUsage PrimaryMarker ("but found" <+> renderMonotype ueActual)
            ]
    diagnosticMarkers (UnresolvedConstraint _ constraint) = [ElaraMarker (constraintLoc constraint) PrimaryMarker (pretty constraint)]
    diagnosticMarkers (PolytypeAlias _) = []

    diagnosticNotes (UnifyError{..}) =
        let contextNotes = fmap (Elara.Error.Note . pretty) (allContexts ueContext)
            unifyNote = Elara.Error.Note ("while unifying" <+> renderMonotype ueExpected <+> "with" <+> renderMonotype ueActual)
            typeNotes =
                [ Elara.Error.Note ("expected type:" <+> renderMonotype ueExpected)
                , Elara.Error.Note ("actual type:  " <+> renderMonotype ueActual)
                ]
            hints = case currentContext ueContext of
                Just (CheckingFunctionArgument pos _ _) ->
                    [Elara.Error.Hint ("Check argument" <+> pretty pos <+> "has the correct type")]
                Just (CheckingIfCondition _) ->
                    [Elara.Error.Hint "The condition of an 'if' expression must have type Bool"]
                Just (CheckingIfBranches _ _) ->
                    [Elara.Error.Hint "Both branches of an 'if' must return the same type"]
                _ -> []
         in contextNotes ++ [unifyNote] ++ typeNotes ++ hints
    diagnosticNotes _ = []
