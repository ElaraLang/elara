module Elara.TypeInfer.Error where

import Data.Map qualified as Map
import Elara.AST.Name (Name)
import Elara.AST.Region (SourceRegion)
import Elara.AST.VarRef (IgnoreLocVarRef)
import Elara.Data.Pretty
import Elara.Error (ReportableError (report), writeReport)
import Elara.TypeInfer.Context (Context)
import Elara.TypeInfer.Existential
import Elara.TypeInfer.Monotype (Monotype)
import Elara.TypeInfer.Monotype qualified as Monotype
import Elara.TypeInfer.Type (Type)
import Elara.TypeInfer.Type qualified as Type
import Error.Diagnose (Report (Err))
import Print (prettyShow)

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
  | UnboundTypeVariable SourceRegion Text
  | UnboundVariable (IgnoreLocVarRef Name)
  | UnboundConstructor (IgnoreLocVarRef Name)
  | --
    RecordTypeMismatch (Type SourceRegion) (Type SourceRegion) (Map.Map Text (Type SourceRegion)) (Map.Map Text (Type SourceRegion))
  | UnionTypeMismatch (Type SourceRegion) (Type SourceRegion) (Map.Map Text (Type SourceRegion)) (Map.Map Text (Type SourceRegion))
  deriving (Eq, Show)

instance ReportableError TypeInferenceError where
  report (MissingVariable a _Γ) =
    writeReport $
      Err
        Nothing
        ( vcat
            [ "Internal error: Invalid context. The following unsolved variable:"
            , pretty a
            , "cannot be solved because the variable is missing from the context"
            , listToText _Γ
            ]
        )
        []
        []
  report e = writeReport $ Err Nothing (prettyShow e) [] []