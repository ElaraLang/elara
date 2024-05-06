{-# LANGUAGE StrictData #-}

module Elara.TypeInfer.Error where

import Data.Containers.ListUtils (nubOrd)
import Data.Map qualified as Map
import Elara.AST.Name (Name, Qualified)
import Elara.AST.Region (SourceRegion, sourceRegionToDiagnosePosition)
import Elara.AST.Shunted
import Elara.AST.VarRef (IgnoreLocVarRef)
import Elara.Data.Kind.Infer (KindInferError)
import Elara.Data.Pretty
import Elara.Error (ReportableError (report), writeReport)
import Elara.TypeInfer.Context (Context, Entry (..))
import Elara.TypeInfer.Existential
import Elara.TypeInfer.Monotype (Monotype)
import Elara.TypeInfer.Monotype qualified as Monotype
import Elara.TypeInfer.Type (Type)
import Elara.TypeInfer.Type qualified as Type
import Elara.TypeInfer.Unique
import Error.Diagnose (Marker (Where), Note (..), Report (Err))
import Print

-- | A data type holding all errors related to type inference
data TypeInferenceError where
    IllFormedAlternatives ::
        SourceRegion ->
        (Existential Monotype.Union) ->
        (Context SourceRegion) ->
        TypeInferenceError
    IllFormedFields ::
        SourceRegion ->
        (Existential Monotype.Record) ->
        (Context SourceRegion) ->
        TypeInferenceError
    IllFormedType :: HasCallStack => SourceRegion -> (Type SourceRegion) -> (Context SourceRegion) -> TypeInferenceError
    InvalidOperands ::
        SourceRegion ->
        (Type SourceRegion) ->
        TypeInferenceError
    MergeConcreteRecord ::
        SourceRegion ->
        (Type SourceRegion) ->
        TypeInferenceError
    MergeInvalidHandler ::
        SourceRegion ->
        (Type SourceRegion) ->
        TypeInferenceError
    MergeRecord ::
        SourceRegion ->
        (Type SourceRegion) ->
        TypeInferenceError
    MissingAllAlternatives ::
        (Existential Monotype.Union) ->
        (Context SourceRegion) ->
        TypeInferenceError
    MissingAllFields ::
        (Existential Monotype.Record) ->
        (Context SourceRegion) ->
        TypeInferenceError
    MissingOneOfAlternatives ::
        [SourceRegion] ->
        (Existential Monotype.Union) ->
        (Existential Monotype.Union) ->
        (Context SourceRegion) ->
        TypeInferenceError
    MissingOneOfFields ::
        [SourceRegion] ->
        (Existential Monotype.Record) ->
        (Existential Monotype.Record) ->
        (Context SourceRegion) ->
        TypeInferenceError
    MissingVariable ::
        HasCallStack =>
        (Existential Monotype) ->
        (Context SourceRegion) ->
        TypeInferenceError
    NotFunctionType ::
        SourceRegion ->
        (Type SourceRegion) ->
        TypeInferenceError
    NotNecessarilyFunctionType ::
        SourceRegion ->
        UniqueTyVar ->
        TypeInferenceError
    NotFieldsSubtype ::
        SourceRegion ->
        (Existential Monotype.Record) ->
        (Type.Record SourceRegion) ->
        TypeInferenceError
    NotRecordSubtype ::
        SourceRegion ->
        (Type SourceRegion) ->
        SourceRegion ->
        (Type SourceRegion) ->
        TypeInferenceError
    NotUnionSubtype ::
        SourceRegion ->
        (Type SourceRegion) ->
        SourceRegion ->
        (Type SourceRegion) ->
        TypeInferenceError
    NotSubtype ::
        HasCallStack =>
        SourceRegion ->
        (Type SourceRegion) ->
        SourceRegion ->
        (Type SourceRegion) ->
        TypeInferenceError
    UnboundAlternatives ::
        SourceRegion ->
        UniqueTyVar ->
        TypeInferenceError
    UnboundFields :: SourceRegion -> UniqueTyVar -> TypeInferenceError
    UnboundTypeVariable ::
        HasCallStack =>
        SourceRegion ->
        UniqueTyVar ->
        (Context SourceRegion) ->
        TypeInferenceError
    UnboundVariable ::
        HasCallStack => SourceRegion -> (IgnoreLocVarRef Name) -> (Context SourceRegion) -> TypeInferenceError
    UnboundConstructor ::
        SourceRegion ->
        (IgnoreLocVarRef Name) ->
        (Context SourceRegion) ->
        TypeInferenceError
    RecordTypeMismatch ::
        (Type SourceRegion) ->
        (Type SourceRegion) ->
        (Map.Map UniqueTyVar (Type SourceRegion)) ->
        (Map.Map UniqueTyVar (Type SourceRegion)) ->
        TypeInferenceError
    UnionTypeMismatch ::
        (Type SourceRegion) ->
        (Type SourceRegion) ->
        (Map.Map UniqueTyVar (Type SourceRegion)) ->
        (Map.Map UniqueTyVar (Type SourceRegion)) ->
        TypeInferenceError
    CustomTypeMismatch ::
        (Type SourceRegion) ->
        (Type SourceRegion) ->
        Qualified Text ->
        Qualified Text ->
        TypeInferenceError
    UserDefinedTypeNotInContext ::
        SourceRegion ->
        ShuntedType ->
        (Context SourceRegion) ->
        TypeInferenceError
    KindInferError :: KindInferError -> TypeInferenceError
    PartiallyAppliedConstructorPattern :: HasCallStack => TypeInferenceError
    NotCustomType :: HasCallStack => SourceRegion -> Type SourceRegion -> TypeInferenceError

deriving instance Show TypeInferenceError

instance Exception TypeInferenceError

instance ReportableError TypeInferenceError where
    report (MissingVariable a _Γ) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Internal error: Invalid context. The following unsolved variable:"
                    , pretty a
                    , "cannot be solved because the variable is missing from the context"
                    , listToText _Γ
                    , pretty $ prettyCallStack callStack
                    ]
                )
                []
                []
    report (UnboundVariable loc v _Γ) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Type error: The following variable is unbound:"
                    , pretty v
                    , "The following variables are bound in the current context:"
                    , listToText _Γ
                    , pretty $ prettyCallStack callStack
                    ]
                )
                [(sourceRegionToDiagnosePosition loc, Where "Referenced here")]
                []
    report (UnboundConstructor loc v _Γ) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Type error: The following constructor is unbound:"
                    , pretty v
                    , "The following constructors are bound in the current context:"
                    , listToText (nubOrd $ filter (\case Annotation{} -> True; _ -> False) _Γ)
                    ]
                )
                [(sourceRegionToDiagnosePosition loc, Where "Referenced here")]
                []
    report (NotSubtype p1 t1 p2 t2) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Type error: The following types are not subtypes:"
                    , pretty t1
                    , "is not a subtype of"
                    , pretty t2
                    , pretty $ prettyCallStack callStack
                    ]
                )
                [ (sourceRegionToDiagnosePosition p1, Where ("Has type" <+> pretty t1))
                , (sourceRegionToDiagnosePosition p2, Where ("Has type" <+> pretty t2))
                ]
                []
    report (UnboundTypeVariable location a _Γ) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Type error: The following type variable is unbound:"
                    , pretty a
                    , "The following type variables are bound in the current context:"
                    , -- , listToText _Γ
                      pretty $ prettyCallStack callStack
                    ]
                )
                [(sourceRegionToDiagnosePosition location, Where "Referenced here")]
                []
    report (UserDefinedTypeNotInContext location a _Γ) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Type error: The following type is not in the current context:"
                    , pretty a
                    , "The following types are bound in the current context:"
                    , listToText _Γ
                    ]
                )
                [(sourceRegionToDiagnosePosition location, Where "Referenced here")]
                []
    report (IllFormedType location _A _Γ) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Type error: The following type is ill-formed:"
                    , pretty _A
                    , "within the current context:"
                    , listToText $ ordNub _Γ
                    , pretty $ prettyCallStack callStack
                    ]
                )
                [(sourceRegionToDiagnosePosition location, Where "Referenced here")]
                []
    report (NotNecessarilyFunctionType loc a) = do
        writeReport $
            Err
                Nothing
                (vsep ["Type error: The following type is not necessarily a function type:", pretty a])
                [(sourceRegionToDiagnosePosition loc, Where "Referenced here")]
                [Note "The type could be anything and so can't be asserted to be a function"]
    report (NotCustomType loc a) = do
        writeReport $
            Err
                Nothing
                (vsep ["Type error: The following type is not a custom type:", pretty a])
                [(sourceRegionToDiagnosePosition loc, Where "Referenced here")]
                []
    report e = writeReport $ Err Nothing (showColored e) [] []
