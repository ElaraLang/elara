{-# LANGUAGE TypeFamilyDependencies #-}

{- | The Frontend AST stage, which is the initial AST produced by the parser.
It's as close to the source code as possible so is quite hard to work with directly.
Notably, this AST stage includes:

  * Let and Def declarations are separate entities
  * Lambdas can have multiple parameters
  * Let bindings can have patterns

Values of these types are produced by "Elara.Parse".
-}
module Elara.AST.Frontend where

import Data.Kind qualified as Kind
import Elara.AST.Generic
import Elara.AST.Generic.Common
import Elara.AST.Name (LowerAlphaName, MaybeQualified, OpName, TypeName, VarName, VarOrConName)
import Elara.AST.Region (Located (..))
import Elara.AST.Select (ASTSelector (..), ForSelector (..), LocatedAST (Frontend))
import Elara.Data.AtLeast2List (AtLeast2List)

-- Generic location and qualification types
type instance ASTLocate' Frontend = Located

type instance ASTQual Frontend = MaybeQualified

{- | Used as a helper type family to reduce boilerplate in the type instances below.
@'SelectFrontend' sel = 'Select' 'Frontend' sel@.
As an explanation of some of the decisions:
- Value declarations do not have a type annotation, because it's handled as the separate 'Elara.AST.Generic.Types.ValueTypeDef' / 'ValueTypeDef'
- Patterns in lambdas and let bindings are lists, since they can have multiple parameters
-}
type family SelectFrontend (selector :: ASTSelector) = (v :: Kind.Type) where
    SelectFrontend (ASTType ForValueDecl) = NoFieldValue
    SelectFrontend (ASTType ForExpr) = Maybe FrontendType
    SelectFrontend LambdaPattern = [FrontendPattern]
    SelectFrontend LetPattern = [FrontendPattern]
    SelectFrontend ASTVarRef = MaybeQualified VarName
    SelectFrontend ConRef = MaybeQualified TypeName
    SelectFrontend LetParamName = VarName
    SelectFrontend InParens = FrontendExpr
    SelectFrontend List = [FrontendExpr]
    SelectFrontend Tuple = AtLeast2List FrontendExpr
    SelectFrontend ASTBinaryOperator = (FrontendBinaryOperator, FrontendExpr, FrontendExpr)
    SelectFrontend TypeApplication = FrontendType
    -- Selections for 'BinaryOperator'
    SelectFrontend SymOp = MaybeQualified OpName
    SelectFrontend Infixed = Located (MaybeQualified VarOrConName)
    -- Selections for 'Pattern'
    SelectFrontend PatternType = Maybe FrontendType
    SelectFrontend VarPat = LowerAlphaName
    SelectFrontend ConPat = MaybeQualified TypeName
    SelectFrontend ListPattern = [FrontendPattern]
    SelectFrontend TuplePattern = NonEmpty FrontendPattern
    SelectFrontend ConsPattern = (FrontendPattern, FrontendPattern)
    -- Selections for 'DeclarationBody'
    SelectFrontend (Patterns ForValueDecl) = [FrontendPattern]
    -- Selections for 'Declaration'
    SelectFrontend (ASTName ForType) = TypeName
    SelectFrontend (ASTName ForValueDecl) = VarName
    -- Selections for 'Type'
    SelectFrontend ASTTypeVar = LowerAlphaName
    SelectFrontend UnitTypeInfo = Located ()
    SelectFrontend TupleType = AtLeast2List FrontendType
    SelectFrontend TypeKind = NoFieldValue
    SelectFrontend UserDefinedType = MaybeQualified TypeName
    SelectFrontend ConstructorName = TypeName
    SelectFrontend Alias = FrontendType
    SelectFrontend ADTParam = FrontendType
    SelectFrontend ValueTypeDef = FrontendType
    SelectFrontend AnnotationName = MaybeQualified TypeName
    SelectFrontend (Annotations _) = [Annotation Frontend]
    SelectFrontend KindAnnotation = NoFieldValue

-- Selections for 'Expr'

type instance Select selector Frontend = SelectFrontend selector

type FrontendExpr = Expr Frontend

type FrontendExpr' = Expr' Frontend

type FrontendPattern = Pattern Frontend

type FrontendPattern' = Pattern' Frontend

type FrontendBinaryOperator = BinaryOperator Frontend

type FrontendBinaryOperator' = BinaryOperator' Frontend

type FrontendType = Type Frontend

type FrontendType' = Type' Frontend

type FrontendDeclaration = Declaration Frontend

type FrontendDeclaration' = Declaration' Frontend

type FrontendDeclarationBody = DeclarationBody Frontend

type FrontendDeclarationBody' = DeclarationBody' Frontend

type FrontendTypeDeclaration = TypeDeclaration Frontend
