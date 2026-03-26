{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Phases.Typed where

import Elara.AST.Name (OpName, Qualified, TypeName, VarName)
import Elara.AST.New.Phase
import Elara.AST.New.Phases.Renamed (TypedLambdaParam)
import Elara.AST.New.Types qualified as AST
import Elara.AST.Region (SourceRegion)
import Elara.AST.VarRef (VarRef)
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Unique (Unique)
import Elara.TypeInfer.Type (Monotype)
import Elara.TypeInfer.Type qualified as Infer
import Elara.TypeInfer.Unique (UniqueTyVar)

type data Typed

instance ElaraPhase Typed where
    type ValueOccurrence Typed loc = Locate loc (VarRef VarName)
    type ConstructorOccurrence Typed loc = Locate loc (Qualified TypeName)
    type TypeOccurrence Typed loc = Locate loc (Qualified TypeName)
    type OperatorOccurrence Typed loc = Locate loc (VarRef OpName)
    type InfixedOccurrence Typed loc = VarRef VarName

    type ValueBinder Typed loc = Locate loc (Unique VarName)
    type TopValueBinder Typed loc = Locate loc (Qualified VarName)
    type TopTypeBinder Typed loc = Locate loc (Qualified TypeName)
    type TypeVariable Typed loc = Locate loc UniqueTyVar
    type ConstructorBinder Typed loc = Locate loc (Qualified TypeName)
    type LambdaBinder Typed loc = TypedLambdaParam (Unique VarName) loc Typed

    type ExpressionMeta Typed loc = Monotype SourceRegion
    type PatternMeta Typed loc = Monotype SourceRegion
    type TypeMeta Typed loc = ElaraKind

    type VariableExtension Typed = Infer.Type SourceRegion
    type LambdaExtension Typed = NoExtension
    type LetExtension Typed = NoExtension
    type ApplicationExtension Typed = NoExtension
    type ConstructorNodeExtension Typed = NoExtension

    type ExpressionExtension Typed loc = Void
    type PatternExtension Typed loc = Void
    type TypeSyntaxExtension Typed loc = Void
    type DeclBodyExtension Typed loc = Void

    type ValueDeclPatterns Typed loc = ()
    type ValueDeclTypeAnnotation Typed loc = ()

    type ValueDeclMetadata Typed loc = Infer.Type SourceRegion
    type TypeDeclMetadata Typed loc = ElaraKind

-- Type aliases (qualified AST to avoid clash with Infer.Type)
type TypedExpr = AST.Expr SourceRegion Typed

type TypedExpr' = AST.Expr' SourceRegion Typed

type TypedPattern = AST.Pattern SourceRegion Typed

type TypedPattern' = AST.Pattern' SourceRegion Typed

type TypedType = AST.Type SourceRegion Typed

type TypedType' = AST.Type' SourceRegion Typed

type TypedDeclaration = AST.Declaration SourceRegion Typed

type TypedDeclaration' = AST.Declaration' SourceRegion Typed

type TypedDeclarationBody = AST.DeclarationBody SourceRegion Typed

type TypedDeclarationBody' = AST.DeclarationBody' SourceRegion Typed

type TypedTypeDeclaration = AST.TypeDeclaration SourceRegion Typed
