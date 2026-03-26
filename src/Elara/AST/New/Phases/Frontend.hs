{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Phases.Frontend where

import Elara.AST.Name (LowerAlphaName, MaybeQualified, OpName, TypeName, VarName, VarOrConName)
import Elara.AST.New.Extensions
import Elara.AST.New.Phase
import Elara.AST.New.Types
import Elara.AST.Region (Located, SourceRegion)

{- | The Frontend AST stage, produced by the parser.
Closest to source syntax: multi-arg lambdas, let-patterns, binary operators, etc.
-}
data Frontend

instance ElaraPhase Frontend where
    -- Occurrences: use Locate so loc=() strips Located wrappers
    type ValueOccurrence Frontend loc = Locate loc (MaybeQualified VarName)
    type ConstructorOccurrence Frontend loc = Locate loc (MaybeQualified TypeName)
    type TypeOccurrence Frontend loc = Locate loc (MaybeQualified TypeName)
    type OperatorOccurrence Frontend loc = Locate loc (MaybeQualified OpName)
    type InfixedOccurrence Frontend loc = Locate loc (MaybeQualified VarOrConName)

    -- Binders: use Locate so loc=() strips Located wrappers
    type ValueBinder Frontend loc = Locate loc VarName
    type TopValueBinder Frontend loc = Locate loc VarName
    type TopTypeBinder Frontend loc = Locate loc TypeName
    type TypeVariable Frontend loc = Locate loc LowerAlphaName
    type ConstructorBinder Frontend loc = Locate loc TypeName
    type LambdaBinder Frontend loc = Pattern loc Frontend -- single pattern (multi-arg uses EMultiLam extension)

    -- Metadata (no analysis data at parse time)
    type ExpressionMeta Frontend loc = ()
    type PatternMeta Frontend loc = Maybe (Type loc Frontend) -- user-written pattern type annotation
    type TypeMeta Frontend loc = ()

    -- Constructor extensions (no extra data at Frontend)
    type VariableExtension Frontend = NoExtension
    type LambdaExtension Frontend = NoExtension
    type LetExtension Frontend = NoExtension
    type ApplicationExtension Frontend = NoExtension
    type ConstructorNodeExtension Frontend = NoExtension

    -- Syntax extensions
    type ExpressionExtension Frontend loc = FrontendExpressionExtension loc
    type PatternExtension Frontend loc = ListTuplePatternExtension loc Frontend
    type TypeSyntaxExtension Frontend loc = TupleTypeExtension loc Frontend
    type DeclBodyExtension Frontend loc = FrontendDeclBodyExtension loc

    -- Declaration metadata (no analysis data)
    type ValueDeclMetadata Frontend loc = NoExtension -- type sig is syntax field, not metadata
    type TypeDeclMetadata Frontend loc = NoExtension

-- | Frontend-specific expression syntax
data FrontendExpressionExtension loc
    = FrontendMultiLam [Pattern loc Frontend] (Expr loc Frontend)
    | FrontendLetWithPatterns (Locate loc VarName) [Pattern loc Frontend] (Expr loc Frontend)
    | FrontendLetInWithPatterns (Locate loc VarName) [Pattern loc Frontend] (Expr loc Frontend) (Expr loc Frontend)
    | FrontendBinaryOperator (BinaryOperatorExtension loc Frontend)
    | FrontendInParens (InParensExtension loc Frontend)
    | FrontendList (ListExprExtension loc Frontend)
    | FrontendTuple (TupleExprExtension loc Frontend)
    deriving (Generic)

-- | Frontend-specific declaration body: separate value type definition
data FrontendDeclBodyExtension loc
    = FrontendValueTypeDef (Locate loc VarName) (Type loc Frontend) [Annotation loc Frontend]
    deriving (Generic)

-- Type aliases for convenience
type FrontendExpr = Expr SourceRegion Frontend
type FrontendExpr' = Expr' SourceRegion Frontend
type FrontendPattern = Pattern SourceRegion Frontend
type FrontendPattern' = Pattern' SourceRegion Frontend
type FrontendType = Type SourceRegion Frontend
type FrontendType' = Type' SourceRegion Frontend
type FrontendDeclaration = Declaration SourceRegion Frontend
type FrontendDeclaration' = Declaration' SourceRegion Frontend
type FrontendDeclarationBody = DeclarationBody SourceRegion Frontend
type FrontendDeclarationBody' = DeclarationBody' SourceRegion Frontend
type FrontendTypeDeclaration = TypeDeclaration SourceRegion Frontend
type FrontendBinaryOperator = BinaryOperator SourceRegion Frontend
