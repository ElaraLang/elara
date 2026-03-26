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
    -- Occurences
    type ValueOccurrence Frontend loc = Locate loc (MaybeQualified VarName)
    type ConstructorOccurrence Frontend loc = Locate loc (MaybeQualified TypeName)
    type TypeOccurrence Frontend loc = Locate loc (MaybeQualified TypeName)
    type OperatorOccurrence Frontend loc = Locate loc (MaybeQualified OpName)
    type InfixedOccurrence Frontend loc = Locate loc (MaybeQualified VarOrConName)

    -- Binders
    type ValueBinder Frontend loc = Locate loc VarName
    type TopValueBinder Frontend loc = Locate loc VarName
    type TopTypeBinder Frontend loc = Locate loc TypeName
    type TypeVariable Frontend loc = Locate loc LowerAlphaName
    type ConstructorBinder Frontend loc = Locate loc TypeName
    type LambdaBinder Frontend loc = Pattern loc Frontend -- single pattern (multi-arg uses EMultiLam extension)

    -- Metadata
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

    -- Value declaration fields (present at Frontend)
    type ValueDeclPatterns Frontend loc = [Pattern loc Frontend]
    type ValueDeclTypeAnnotation Frontend loc = Maybe (Type loc Frontend)

    -- Declaration metadata (no analysis data)
    type ValueDeclMetadata Frontend loc = NoExtension -- type sig is syntax field, not metadata
    type TypeDeclMetadata Frontend loc = NoExtension

-- | Frontend-specific expression syntax
data FrontendExpressionExtension loc
    = -- | Lambda with multiple patterns (desugared into nested lambdas) e.g. @\ x y-> ...@
      FrontendMultiLam [Pattern loc Frontend] (Expr loc Frontend)
    | -- | Let with pattern binding (desugared into let + pattern match) e.g. @let (x, y) = pair in ...@
      FrontendLetWithPatterns (Locate loc VarName) [Pattern loc Frontend] (Expr loc Frontend)
    | -- | Let-in with pattern binding
      FrontendLetInWithPatterns (Locate loc VarName) [Pattern loc Frontend] (Expr loc Frontend) (Expr loc Frontend)
    | -- | Binary operator application e.g. @x + y@
      FrontendBinaryOperator (BinaryOperatorExtension loc Frontend)
    | -- | In-parens expression (used to preserve parentheses for correct desugaring of operators)
      FrontendInParens (InParensExtension loc Frontend)
    | -- | List literal e.g. @[x, y, z]@
      FrontendList (ListExprExtension loc Frontend)
    | -- | Tuple literal e.g. @(x, y)@
      FrontendTuple (TupleExprExtension loc Frontend)
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
