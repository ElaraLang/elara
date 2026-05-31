module Elara.JVM.Error (JVMLoweringError (..)) where

import H2JVM (MethodDescriptor)
import H2JVM.Analyse.StackMap

import Elara.AST.Name
import Elara.Core.Pretty ()
import Elara.Data.Pretty
import Elara.Error

import Elara.Core qualified as Core

-- | Errors that can occur during JVM lowering and code generation
data JVMLoweringError
    = -- | Expected a local variable in a lambda, but got something else
      LambdaBinderNotLocal Core.Var
    | -- | A data constructor has more lambda abstractions than type parameters
      MoreLambdasThanTypeArgs Text Int Int
    | -- | A data constructor was applied to the wrong number of arguments
      DataConArityMismatch Text Int Int
    | -- | Expected a functional type, but got something else
      NotAFunctionalType Core.Type
    | -- | A type constructor application was encountered that we don't know how to lower
      UnsupportedType Core.Type
    | -- | A variable was encountered that isn't bound in the current scope
      LocalVariableNotFound Core.Var
    | -- | Tried to invoke a static method as a local variable
      InvokeStaticLocal Text
    | -- | Unresolved reference to a class
      ClassNotFound Text
    | -- | Unresolved reference to a method
      MethodNotFound Text Text MethodDescriptor
    | -- | Unresolved reference to a field
      FieldNotFound Text Text Core.Type
    | -- | A global variable was found in a pattern match (not supported yet)
      GlobalVarInPattern (Qualified Text)
    | -- | A literal was encountered that we don't know how to lower
      UnsupportedLiteral Core.Literal
    | -- | A type variable was found in a pattern match (not supported)
      TypeVarInPattern
    | -- | A callable was applied to too many arguments
      OverApplicationOf (Text, MethodDescriptor) Int Int
    | -- | An expression of an unsupported type was encountered
      UnsupportedExpressionType Core.CoreExpr
    | -- | A non-function was applied to an argument
      AppOfNonFunction Text
    | MethodTooManyLocals Int
    | MethodTooManyStack Int
    | H2JVMError StackMapError
    deriving (Show, Typeable)

instance Exception JVMLoweringError

instance Pretty JVMLoweringError where
    pretty (LambdaBinderNotLocal v) =
        "Lambda binder is not local:" <+> pretty v
    pretty (MoreLambdasThanTypeArgs name expected actual) =
        "Data constructor" <+> pretty name <+> "has more lambda abstractions (" <> pretty actual <> ") than type parameters (" <> pretty expected <> ")"
    pretty (DataConArityMismatch name expected actual) =
        "Data constructor" <+> pretty name <+> "expects" <+> pretty expected <+> "arguments, but got" <+> pretty actual
    pretty (NotAFunctionalType t) =
        "Expected a functional type, but got:" <+> pretty t
    pretty (UnsupportedType app) =
        "Unsupported type application:" <+> pretty app
    pretty (LocalVariableNotFound v) =
        "Local variable not found:" <+> pretty v
    pretty (InvokeStaticLocal name) =
        "Cannot invoke static method" <+> pretty name <+> "as a local variable"
    pretty (ClassNotFound name) =
        "Class not found:" <+> pretty name
    pretty (MethodNotFound cls name desc) =
        "Method not found:" <+> pretty cls <> "." <> pretty name <> pretty desc
    pretty (FieldNotFound cls name t) =
        "Field not found:" <+> pretty cls <> "." <> pretty name <+> ":" <+> pretty t
    pretty (GlobalVarInPattern name) =
        "Global variable found in pattern:" <+> pretty name
    pretty (UnsupportedLiteral lit) =
        "Unsupported literal type:" <+> pretty lit
    pretty TypeVarInPattern =
        "Type variable found in pattern (not supported)"
    pretty (OverApplicationOf (name, desc) actual expected) =
        "Over-application of callable" <+> pretty name <+> pretty desc <> ". Expected" <+> pretty expected <+> "arguments, but got" <+> pretty actual
    pretty (UnsupportedExpressionType e) =
        "Unsupported expression type:" <+> pretty e
    pretty (AppOfNonFunction e) =
        "Application of non-function:" <+> pretty e
    pretty (MethodTooManyLocals n) =
        "Method has too many local variables:" <+> pretty n <> ". The JVM limit is 65535."
    pretty (MethodTooManyStack n) =
        "Method has too many stack entries:" <+> pretty n <> ". The JVM limit is 65535."

instance ElaraDiagnostic JVMLoweringError where
    diagnosticMessage = pretty
