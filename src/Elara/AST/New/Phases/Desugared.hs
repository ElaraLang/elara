{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Phases.Desugared where

import Elara.AST.Name (LowerAlphaName, MaybeQualified, OpName, TypeName, VarName, VarOrConName)
import Elara.AST.New.Extensions
import Elara.AST.New.Phase
import Elara.AST.New.Types
import Elara.AST.Region (SourceRegion)
import Elara.Data.AtLeast2List (AtLeast2List)

{- | Desugared AST stage. Key changes from Frontend:
* Lambdas have exactly 1 argument (multi-arg desugared into nested)
* Let bindings have no patterns (desugared into lambdas)
* Def and Let declarations are merged
-}
data Desugared

instance ElaraPhase Desugared where
    -- Occurrences (same as Frontend)
    type ValueOccurrence Desugared loc = Locate loc (MaybeQualified VarName)
    type ConstructorOccurrence Desugared loc = Locate loc (MaybeQualified TypeName)
    type TypeOccurrence Desugared loc = Locate loc (MaybeQualified TypeName)
    type OperatorOccurrence Desugared loc = Locate loc (MaybeQualified OpName)
    type InfixedOccurrence Desugared loc = Locate loc (MaybeQualified VarOrConName)

    -- Binders (same as Frontend)
    type ValueBinder Desugared loc = Locate loc VarName
    type TopValueBinder Desugared loc = Locate loc VarName
    type TopTypeBinder Desugared loc = Locate loc TypeName
    type TypeVariable Desugared loc = Locate loc LowerAlphaName
    type ConstructorBinder Desugared loc = Locate loc TypeName
    type LambdaBinder Desugared loc = Pattern loc Desugared -- always single pattern now

    -- Metadata
    type ExpressionMeta Desugared loc = ()
    type PatternMeta Desugared loc = Maybe (Type loc Desugared)
    type TypeMeta Desugared loc = ()

    -- Constructor extensions (no extra data)
    type VariableExtension Desugared = NoExtension
    type LambdaExtension Desugared = NoExtension
    type LetExtension Desugared = NoExtension
    type ApplicationExtension Desugared = NoExtension
    type ConstructorNodeExtension Desugared = NoExtension

    -- Syntax extensions (still has BinOp, InParens, List, Tuple but NOT multi-lam or let-patterns)
    type ExpressionExtension Desugared loc = DesugaredExpressionExtension loc
    type PatternExtension Desugared loc = ListTuplePatternExtension loc Desugared
    type TypeSyntaxExtension Desugared loc = TupleTypeExtension loc Desugared
    type DeclBodyExtension Desugared loc = Void -- ValueTypeDef merged during desugar

    -- Value declaration fields (eliminated after desugar)
    type ValueDeclPatterns Desugared loc = ()
    type ValueDeclTypeAnnotation Desugared loc = ()

    -- Declaration metadata
    type ValueDeclMetadata Desugared loc = Maybe (Type loc Desugared) -- merged type signature
    type TypeDeclMetadata Desugared loc = NoExtension

-- | Desugared-specific expression syntax (no multi-lam, no let-patterns)
data DesugaredExpressionExtension loc
    = DesugaredBinaryOperator (BinaryOperatorExtension loc Desugared)
    | DesugaredInParens (InParensExtension loc Desugared)
    | DesugaredList (ListExprExtension loc Desugared)
    | DesugaredTuple (TupleExprExtension loc Desugared)
    deriving (Generic)

-- Type aliases
type DesugaredExpr = Expr SourceRegion Desugared
type DesugaredExpr' = Expr' SourceRegion Desugared
type DesugaredPattern = Pattern SourceRegion Desugared
type DesugaredPattern' = Pattern' SourceRegion Desugared
type DesugaredType = Type SourceRegion Desugared
type DesugaredType' = Type' SourceRegion Desugared
type DesugaredDeclaration = Declaration SourceRegion Desugared
type DesugaredDeclaration' = Declaration' SourceRegion Desugared
type DesugaredDeclarationBody = DeclarationBody SourceRegion Desugared
type DesugaredDeclarationBody' = DeclarationBody' SourceRegion Desugared
type DesugaredTypeDeclaration = TypeDeclaration SourceRegion Desugared
type DesugaredBinaryOperator = BinaryOperator SourceRegion Desugared
