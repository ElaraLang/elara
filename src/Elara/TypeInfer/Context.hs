-- | Inference context tracking for better error messages
module Elara.TypeInfer.Context where

import Elara.AST.Name (Qualified, VarName)
import Elara.AST.Region (SourceRegion)
import Elara.Data.Pretty

{- | Describes the context in which type inference is happening.
This is used to provide better error messages by explaining
WHY two types are being compared.
-}
data InferenceContext
    = -- | Checking an argument to a function call
      CheckingFunctionArgument
        { argPosition :: !Int
        , functionName :: !(Maybe (Qualified VarName))
        , callSite :: !SourceRegion
        }
    | -- | Checking the result type of a function call
      CheckingFunctionResult
        { callSite :: !SourceRegion
        }
    | -- | Checking the condition of an if expression
      CheckingIfCondition
        { ifSite :: !SourceRegion
        }
    | -- | Checking that if branches have the same type
      CheckingIfBranches
        { thenSite :: !SourceRegion
        , elseSite :: !SourceRegion
        }
    | -- | Checking a branch in a match expression
      CheckingMatchBranch
        { branchIndex :: !Int
        , branchSite :: !SourceRegion
        }
    | -- | Checking a pattern
      CheckingPattern
        { patternSite :: !SourceRegion
        }
    | -- | Checking a let binding
      CheckingLetBinding
        { bindingName :: !(Qualified VarName)
        , bindingSite :: !SourceRegion
        }
    | -- | Checking against a type annotation
      CheckingAnnotation
        { annotationSite :: !SourceRegion
        }
    deriving (Generic, Show, Eq, Ord)

-- | A stack of inference contexts, with the most recent context at the head.
newtype ContextStack = ContextStack [InferenceContext]
    deriving (Generic, Show, Eq)
    deriving newtype (Semigroup, Monoid)

-- | Create an empty context stack
emptyContextStack :: ContextStack
emptyContextStack = ContextStack []

-- | Push a context onto the stack
pushContext :: InferenceContext -> ContextStack -> ContextStack
pushContext ctx (ContextStack stack) = ContextStack (ctx : stack)

-- | Get the current (most recent) context, if any
currentContext :: ContextStack -> Maybe InferenceContext
currentContext (ContextStack []) = Nothing
currentContext (ContextStack (x : _)) = Just x

-- | Get all contexts in the stack (most recent first)
allContexts :: ContextStack -> [InferenceContext]
allContexts (ContextStack stack) = stack

instance Pretty InferenceContext where
    pretty = \case
        CheckingFunctionArgument pos mFn _ ->
            "while checking argument" <+> pretty pos <+> maybe mempty (\fn -> "of" <+> squotes (pretty fn)) mFn
        CheckingFunctionResult _ ->
            "while checking function result"
        CheckingIfCondition _ ->
            "while checking if condition"
        CheckingIfBranches _ _ ->
            "while checking that if branches have the same type"
        CheckingMatchBranch idx _ ->
            "while checking match branch" <+> pretty idx
        CheckingPattern _ ->
            "while checking pattern"
        CheckingLetBinding name _ ->
            "while checking let binding" <+> squotes (pretty name)
        CheckingAnnotation _ ->
            "while checking type annotation"

instance Pretty ContextStack where
    pretty (ContextStack []) = mempty
    pretty (ContextStack ctxs) = vsep (pretty <$> ctxs)
