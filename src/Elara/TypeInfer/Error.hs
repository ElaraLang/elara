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
import Error.Diagnose (Marker (Where), Report (Err))
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
    | NotNecessarilyFunctionType SourceRegion Text
    | --
      NotAlternativesSubtype SourceRegion (Existential Monotype.Union) (Type.Union SourceRegion)
    | NotFieldsSubtype SourceRegion (Existential Monotype.Record) (Type.Record SourceRegion)
    | NotRecordSubtype SourceRegion (Type SourceRegion) SourceRegion (Type SourceRegion)
    | NotUnionSubtype SourceRegion (Type SourceRegion) SourceRegion (Type SourceRegion)
    | NotSubtype SourceRegion (Type SourceRegion) SourceRegion (Type SourceRegion)
    | --
      UnboundAlternatives SourceRegion Text
    | UnboundFields SourceRegion Text
    | UnboundTypeVariable SourceRegion Text (Context SourceRegion)
    | UnboundVariable
        SourceRegion
        -- ^ Location of the variable that caused the error
        (IgnoreLocVarRef Name)
        (Context SourceRegion)
    | UnboundConstructor (IgnoreLocVarRef Name) (Context SourceRegion)
    | --
      RecordTypeMismatch (Type SourceRegion) (Type SourceRegion) (Map.Map Text (Type SourceRegion)) (Map.Map Text (Type SourceRegion))
    | UnionTypeMismatch (Type SourceRegion) (Type SourceRegion) (Map.Map Text (Type SourceRegion)) (Map.Map Text (Type SourceRegion))
    | --
      UserDefinedTypeNotInContext SourceRegion ShuntedType (Context SourceRegion)
    | KindInferError KindInferError
    deriving (Show)

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
    report (NotSubtype _ t1 _ t2) =
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
                []
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
    report e = writeReport $ Err Nothing (showColored e) [] []
