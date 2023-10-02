module Elara.TypeInfer.Error where

import Data.Map qualified as Map
import Elara.AST.Name (Name)
import Elara.AST.Region (SourceRegion, sourceRegionToDiagnosePosition)
import Elara.AST.Shunted
import Elara.AST.VarRef (IgnoreLocVarRef)
import Elara.Data.Kind.Infer (KindInferError)
import Elara.Data.Pretty
import Elara.Error (ReportableError (report), writeReport)
import Elara.TypeInfer.Context (Context)
import Elara.TypeInfer.Existential
import Elara.TypeInfer.Monotype (Monotype)
import Elara.TypeInfer.Monotype qualified as Monotype
import Elara.TypeInfer.Type (Type)
import Elara.TypeInfer.Type qualified as Type
import Elara.TypeInfer.Unique
import Error.Diagnose (Marker (Where), Note (..), Report (Err))
import Print

-- | A data type holding all errors related to type inference
data TypeInferenceError
    = IllFormedAlternatives SourceRegion (Existential Monotype.Union) (Context SourceRegion)
    | IllFormedFields SourceRegion (Existential Monotype.Record) (Context SourceRegion)
    | IllFormedType SourceRegion (Type SourceRegion) (Context SourceRegion)
    | --
      InvalidOperands SourceRegion (Type SourceRegion)
    | --
      MergeConcreteRecord SourceRegion (Type SourceRegion)
    | MergeInvalidHandler SourceRegion (Type SourceRegion)
    | MergeRecord SourceRegion (Type SourceRegion)
    | --
      MissingAllAlternatives (Existential Monotype.Union) (Context SourceRegion)
    | MissingAllFields (Existential Monotype.Record) (Context SourceRegion)
    | MissingOneOfAlternatives [SourceRegion] (Existential Monotype.Union) (Existential Monotype.Union) (Context SourceRegion)
    | MissingOneOfFields [SourceRegion] (Existential Monotype.Record) (Existential Monotype.Record) (Context SourceRegion)
    | MissingVariable (Existential Monotype) (Context SourceRegion)
    | --
      NotFunctionType SourceRegion (Type SourceRegion)
    | NotNecessarilyFunctionType SourceRegion UniqueTyVar
    | --
      NotAlternativesSubtype SourceRegion (Existential Monotype.Union) (Type.Union SourceRegion)
    | NotFieldsSubtype SourceRegion (Existential Monotype.Record) (Type.Record SourceRegion)
    | NotRecordSubtype SourceRegion (Type SourceRegion) SourceRegion (Type SourceRegion)
    | NotUnionSubtype SourceRegion (Type SourceRegion) SourceRegion (Type SourceRegion)
    | NotSubtype SourceRegion (Type SourceRegion) SourceRegion (Type SourceRegion)
    | --
      UnboundAlternatives SourceRegion UniqueTyVar
    | UnboundFields SourceRegion UniqueTyVar
    | UnboundTypeVariable SourceRegion UniqueTyVar (Context SourceRegion)
    | UnboundVariable
        SourceRegion
        -- ^ Location of the variable that caused the error
        (IgnoreLocVarRef Name)
        (Context SourceRegion)
    | UnboundConstructor (IgnoreLocVarRef Name) (Context SourceRegion)
    | --
      RecordTypeMismatch (Type SourceRegion) (Type SourceRegion) (Map.Map UniqueTyVar (Type SourceRegion)) (Map.Map UniqueTyVar (Type SourceRegion))
    | UnionTypeMismatch (Type SourceRegion) (Type SourceRegion) (Map.Map UniqueTyVar (Type SourceRegion)) (Map.Map UniqueTyVar (Type SourceRegion))
    | CustomTypeMismatch (Type SourceRegion) (Type SourceRegion) Text Text
    | --
      UserDefinedTypeNotInContext SourceRegion ShuntedType (Context SourceRegion)
    | KindInferError KindInferError
    deriving (Show)

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
                    ]
                )
                [(sourceRegionToDiagnosePosition loc, Where "Referenced here")]
                []
    report (UnboundConstructor v _Γ) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Type error: The following constructor is unbound:"
                    , pretty v
                    , "The following constructors are bound in the current context:"
                    , listToText _Γ
                    ]
                )
                []
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
                    ]
                )
                [(sourceRegionToDiagnosePosition p1, Where (pretty t1)), (sourceRegionToDiagnosePosition p2, Where (pretty t2))]
                []
    report (UnboundTypeVariable location a _Γ) =
        writeReport $
            Err
                Nothing
                ( vsep
                    [ "Type error: The following type variable is unbound:"
                    , pretty a
                    , "The following type variables are bound in the current context:"
                    , listToText _Γ
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
                    , listToText _Γ
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
    report e = writeReport $ Err Nothing (showColored e) [] []
