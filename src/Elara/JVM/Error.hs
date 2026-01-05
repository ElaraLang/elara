module Elara.JVM.Error where

import Elara.AST.Name
import Elara.Core qualified as Core
import Elara.Core.Pretty ()
import Elara.Data.Pretty
import Elara.Error (ReportableError)
import JVM.Data.Abstract.Descriptor (MethodDescriptor)

-- | Errors that can occur during JVM lowering and code generation
data JVMLoweringError
    = -- | Expected a local variable in a lambda, but got something else
      LambdaBinderNotLocal Core.Var
    | -- | A data constructor has more lambda abstractions than type parameters
      MoreLambasAthanTypeArgs Text Int Int
    | -- | Unknown primitive operation name
      UnknownPrimitiveKey Text
    | -- | Unknown primitive type in type signature
      UnknownPrimitiveType (Qualified Text)
    | -- | Type variable found in an expression context where a concrete type is required
      TypeVarInExpr Core.Type
    | -- | Expression type cannot be lowered to JVM
      UnsupportedExpressionType Core.CoreExpr
    | -- | Over-application: more arguments provided than the function expects
      OverApplicationOf
        -- | Function name
        (Text, MethodDescriptor)
        -- | Expected arguments
        Int
        -- | Actual arguments
        Int
    | -- | Data constructor over-application
      DataConOverApplication (Qualified Text) Int Int
    | -- | Application of non-function value
      AppOfNonFunction Text
    | -- | Pattern matching on non-data type
      PatternMatchOnNonData Core.Type
    | -- | Type variable in pattern binding (shouldn't happen in Core)
      TypeVarInPattern
    | GlobalVarInPattern (Qualified Text)
    | -- | Unsupported literal type
      UnsupportedLiteral Core.Literal
    deriving (Typeable, Show, Generic)

instance Exception JVMLoweringError

instance Pretty JVMLoweringError where
    pretty (LambdaBinderNotLocal var) =
        "Lambda binder must be a local identifier, but got:" <+> pretty var
    pretty (MoreLambasAthanTypeArgs name numLambdas numTypes) =
        "More lambda abstractions than type parameters for" <+> pretty name
            <> colon
                <+> pretty numLambdas
                <+> "lambdas vs"
                <+> pretty numTypes
                <+> "type parameters"
    pretty (UnknownPrimitiveKey key) =
        "Unknown primitive operation:" <+> pretty key
    pretty (UnknownPrimitiveType qname) =
        "Unknown primitive type:" <+> pretty qname
    pretty (TypeVarInExpr ty) =
        "Type variable found in expression context:" <+> pretty ty
    pretty (UnsupportedExpressionType desc) =
        "Unsupported expression type:" <+> pretty desc
    pretty (OverApplicationOf qname expected actual) =
        "Over-application of" <+> pretty qname
            <> colon
                <+> "expected"
                <+> pretty expected
                <+> "arguments but got"
                <+> pretty actual
    pretty (DataConOverApplication con expected actual) =
        "Data constructor over-application:"
            <+> pretty con
            <+> "expects"
            <+> pretty expected
            <+> "but got"
            <+> pretty actual
    pretty (AppOfNonFunction name) =
        "Attempted to apply non-function value:" <+> pretty name
    pretty (PatternMatchOnNonData ty) =
        "Pattern match on non-data type:" <+> pretty ty
    pretty TypeVarInPattern =
        "Type variable found in pattern (internal error)"
    pretty (GlobalVarInPattern name) =
        "Global variable found in pattern:" <+> pretty name
    pretty (UnsupportedLiteral lit) =
        "Unsupported literal type:" <+> pretty lit

instance ReportableError JVMLoweringError
