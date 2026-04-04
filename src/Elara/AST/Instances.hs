{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Elara.AST.Instances where

import Data.Kind qualified as Kind
import Elara.AST.Extensions
import Elara.AST.Module
import Elara.AST.Name (LowerAlphaName, ModuleName)
import Elara.AST.Phase
import Elara.AST.Phases.Desugared (Desugared, DesugaredExpressionExtension (..))
import Elara.AST.Phases.Frontend (Frontend, FrontendDeclBodyExtension (..), FrontendExpressionExtension (..))
import Elara.AST.Phases.Frontend.Pretty ()
import Elara.AST.Phases.Renamed (Renamed, RenamedExpressionExtension (..))
import Elara.AST.Pretty
import Elara.AST.Types
import Elara.Data.Pretty
import GHC.Generics (Rep)

-- | Constraint alias for all the type family components needed for Show/Eq/Ord
type CoreConstraint (c :: Kind.Type -> Kind.Constraint) loc p =
    ( c loc
    , c (ExpressionMeta p loc)
    , c (PatternMeta p loc)
    , c (TypeMeta p loc)
    , c (ValueOccurrence p loc)
    , c (ConstructorOccurrence p loc)
    , c (TypeOccurrence p loc)
    , c (OperatorOccurrence p loc)
    , c (InfixedOccurrence p loc)
    , c (ValueBinder p loc)
    , c (TopValueBinder p loc)
    , c (TopTypeBinder p loc)
    , c (TypeVariable p loc)
    , c (ConstructorBinder p loc)
    , c (Locate loc LowerAlphaName)
    , c (Locate loc ModuleName)
    , c (LambdaBinder p loc)
    , c (VariableExtension p)
    , c (LambdaExtension p)
    , c (LetExtension p)
    , c (ApplicationExtension p)
    , c (ConstructorNodeExtension p)
    , c (ExpressionExtension p loc)
    , c (PatternExtension p loc)
    , c (TypeSyntaxExtension p loc)
    , c (DeclBodyExtension p loc)
    , c (ValueDeclPatterns p loc)
    , c (ValueDeclTypeAnnotation p loc)
    , c (ValueDeclMetadata p loc)
    , c (TypeDeclMetadata p loc)
    )

-- Show instances
deriving instance CoreConstraint Show loc p => Show (Expr loc p)

deriving instance CoreConstraint Show loc p => Show (Expr' loc p)

deriving instance CoreConstraint Show loc p => Show (Pattern loc p)

deriving instance CoreConstraint Show loc p => Show (Pattern' loc p)

deriving instance CoreConstraint Show loc p => Show (Type loc p)

deriving instance CoreConstraint Show loc p => Show (Type' loc p)

deriving instance CoreConstraint Show loc p => Show (BinaryOperator loc p)

deriving instance CoreConstraint Show loc p => Show (TypeDeclaration loc p)

deriving instance CoreConstraint Show loc p => Show (Annotation loc p)

deriving instance CoreConstraint Show loc p => Show (AnnotationArg loc p)

deriving instance CoreConstraint Show loc p => Show (Declaration loc p)

deriving instance CoreConstraint Show loc p => Show (Declaration' loc p)

deriving instance CoreConstraint Show loc p => Show (DeclarationBody loc p)

deriving instance CoreConstraint Show loc p => Show (DeclarationBody' loc p)

-- Eq instances
deriving instance CoreConstraint Eq loc p => Eq (Expr loc p)

deriving instance CoreConstraint Eq loc p => Eq (Expr' loc p)

deriving instance CoreConstraint Eq loc p => Eq (Pattern loc p)

deriving instance CoreConstraint Eq loc p => Eq (Pattern' loc p)

deriving instance CoreConstraint Eq loc p => Eq (Type loc p)

deriving instance CoreConstraint Eq loc p => Eq (Type' loc p)

deriving instance CoreConstraint Eq loc p => Eq (BinaryOperator loc p)

deriving instance CoreConstraint Eq loc p => Eq (TypeDeclaration loc p)

deriving instance CoreConstraint Eq loc p => Eq (Annotation loc p)

deriving instance CoreConstraint Eq loc p => Eq (AnnotationArg loc p)

deriving instance CoreConstraint Eq loc p => Eq (Declaration loc p)

deriving instance CoreConstraint Eq loc p => Eq (Declaration' loc p)

deriving instance CoreConstraint Eq loc p => Eq (DeclarationBody loc p)

deriving instance CoreConstraint Eq loc p => Eq (DeclarationBody' loc p)

-- Ord instances
deriving instance CoreConstraint Ord loc p => Ord (Expr loc p)

deriving instance CoreConstraint Ord loc p => Ord (Expr' loc p)

deriving instance CoreConstraint Ord loc p => Ord (Pattern loc p)

deriving instance CoreConstraint Ord loc p => Ord (Pattern' loc p)

deriving instance CoreConstraint Ord loc p => Ord (Type loc p)

deriving instance CoreConstraint Ord loc p => Ord (Type' loc p)

deriving instance CoreConstraint Ord loc p => Ord (BinaryOperator loc p)

deriving instance CoreConstraint Ord loc p => Ord (TypeDeclaration loc p)

deriving instance CoreConstraint Ord loc p => Ord (Annotation loc p)

deriving instance CoreConstraint Ord loc p => Ord (AnnotationArg loc p)

deriving instance CoreConstraint Ord loc p => Ord (Declaration loc p)

deriving instance CoreConstraint Ord loc p => Ord (Declaration' loc p)

deriving instance CoreConstraint Ord loc p => Ord (DeclarationBody loc p)

deriving instance CoreConstraint Ord loc p => Ord (DeclarationBody' loc p)

-- Extension type instances
deriving instance CoreConstraint Show loc p => Show (BinaryOperatorExtension loc p)

deriving instance CoreConstraint Eq loc p => Eq (BinaryOperatorExtension loc p)

deriving instance CoreConstraint Ord loc p => Ord (BinaryOperatorExtension loc p)

deriving instance CoreConstraint Show loc p => Show (InParensExtension loc p)

deriving instance CoreConstraint Eq loc p => Eq (InParensExtension loc p)

deriving instance CoreConstraint Ord loc p => Ord (InParensExtension loc p)

deriving instance CoreConstraint Show loc p => Show (ListExprExtension loc p)

deriving instance CoreConstraint Eq loc p => Eq (ListExprExtension loc p)

deriving instance CoreConstraint Ord loc p => Ord (ListExprExtension loc p)

deriving instance CoreConstraint Show loc p => Show (TupleExprExtension loc p)

deriving instance CoreConstraint Eq loc p => Eq (TupleExprExtension loc p)

deriving instance CoreConstraint Ord loc p => Ord (TupleExprExtension loc p)

deriving instance CoreConstraint Show loc p => Show (ListTuplePatternExtension loc p)

deriving instance CoreConstraint Eq loc p => Eq (ListTuplePatternExtension loc p)

deriving instance CoreConstraint Ord loc p => Ord (ListTuplePatternExtension loc p)

deriving instance CoreConstraint Show loc p => Show (TupleTypeExtension loc p)

deriving instance CoreConstraint Eq loc p => Eq (TupleTypeExtension loc p)

deriving instance CoreConstraint Ord loc p => Ord (TupleTypeExtension loc p)

-- Frontend-specific extension instances
-- These use CoreConstraint with Frontend hardcoded, which GHC resolves via UndecidableInstances
deriving instance CoreConstraint Show loc Frontend => Show (FrontendExpressionExtension loc)

deriving instance CoreConstraint Eq loc Frontend => Eq (FrontendExpressionExtension loc)

deriving instance CoreConstraint Ord loc Frontend => Ord (FrontendExpressionExtension loc)

deriving instance CoreConstraint Show loc Frontend => Show (FrontendDeclBodyExtension loc)

deriving instance CoreConstraint Eq loc Frontend => Eq (FrontendDeclBodyExtension loc)

deriving instance CoreConstraint Ord loc Frontend => Ord (FrontendDeclBodyExtension loc)

-- Desugared-specific extension instances
deriving instance CoreConstraint Show loc Desugared => Show (DesugaredExpressionExtension loc)

deriving instance CoreConstraint Eq loc Desugared => Eq (DesugaredExpressionExtension loc)

deriving instance CoreConstraint Ord loc Desugared => Ord (DesugaredExpressionExtension loc)

-- Renamed-specific extension instances
deriving instance CoreConstraint Show loc Renamed => Show (RenamedExpressionExtension loc)

deriving instance CoreConstraint Eq loc Renamed => Eq (RenamedExpressionExtension loc)

deriving instance CoreConstraint Ord loc Renamed => Ord (RenamedExpressionExtension loc)

-- TypedLambdaParam instances
deriving instance (Show v, Show (PatternMeta p loc)) => Show (TypedLambdaParam v loc p)

deriving instance (Eq v, Eq (PatternMeta p loc)) => Eq (TypedLambdaParam v loc p)

deriving instance (Ord v, Ord (PatternMeta p loc)) => Ord (TypedLambdaParam v loc p)

-- Module instances
deriving instance CoreConstraint Show loc p => Show (Module loc p)

deriving instance CoreConstraint Show loc p => Show (Module' loc p)

deriving instance CoreConstraint Show loc p => Show (Import loc p)

deriving instance CoreConstraint Show loc p => Show (Import' loc p)

deriving instance CoreConstraint Show loc p => Show (ImportExposingOrHiding loc p)

deriving instance CoreConstraint Show loc p => Show (Exposing loc p)

deriving instance CoreConstraint Show loc p => Show (Exposition loc p)

-- Pretty instances for core AST types. These live here to avoid cyclic imports between Module and Pretty.
instance (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc, Pretty (Locate loc ModuleName)) => Pretty (Module loc p) where
    pretty = prettyModule

instance (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Pretty (Declaration loc p) where
    pretty = prettyDeclaration

instance (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Pretty (DeclarationBody' loc p) where
    pretty = prettyDeclarationBody

instance (PrettyPhase p, PrettyExtensions p, PrettyPhaseLoc p loc) => Pretty (TypeDeclaration loc p) where
    pretty = prettyTypeDeclaration

instance (PrettyPhase p, PrettyPhaseLoc p loc) => Pretty (BinaryOperator loc p) where
    pretty = prettyBinaryOperator

instance
    forall c loc phase.
    ( Generic c
    , SafeGPlate (Rep c) (Expr loc phase)
    , GPlate (Expr loc phase) c
    ) =>
    Plated (Expr loc phase) c

instance
    forall c loc phase.
    ( Generic c
    , SafeGPlate (Rep c) (Pattern loc phase)
    , GPlate (Pattern loc phase) c
    ) =>
    Plated (Pattern loc phase) c

instance
    forall c loc phase.
    ( Generic c
    , SafeGPlate (Rep c) (Type loc phase)
    , GPlate (Type loc phase) c
    ) =>
    Plated (Type loc phase) c
