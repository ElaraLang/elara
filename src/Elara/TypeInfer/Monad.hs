module Elara.TypeInfer.Monad where

import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local
import Elara.Data.Pretty
import Elara.Data.Unique.Effect
import Elara.Error.Effect (DiagnosticWriter)
import Elara.Logging
import Elara.Query qualified
import Elara.Query.Effects
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Type (Constraint)
import Rock qualified

type InferEffectsCons loc xs =
    Writer (Constraint loc)
        ': State (LocalTypeEnvironment loc)
        ': State (TypeEnvironment loc)
        ': Error (InferError loc)
        ': UniqueGen
        ': StructuredDebug
        ': DiagnosticWriter (Doc AnsiStyle)
        ': Rock.Rock Elara.Query.Query
        ': xs

type Infer loc r =
    ( Writer (Constraint loc) :> r
    , State (LocalTypeEnvironment loc) :> r
    , State (TypeEnvironment loc) :> r
    , Error (InferError loc) :> r
    , Rock.Rock Elara.Query.Query :> r
    , QueryEffects r
    )
