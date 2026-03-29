{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Phases.MidKinded where

import Elara.AST.Name (LowerAlphaName, OpName, Qualified, TypeName, VarName)
import Elara.AST.New.Phase
import Elara.AST.New.Types
import Elara.AST.Region (SourceRegion)
import Elara.AST.VarRef (VarRef)
import Elara.Data.Unique (Unique, UniqueId)

data MidKinded

instance ElaraPhase MidKinded where
    type ValueOccurrence MidKinded loc = Locate loc (VarRef VarName)
    type ConstructorOccurrence MidKinded loc = Locate loc (Qualified TypeName)
    type TypeOccurrence MidKinded loc = Locate loc (Qualified TypeName)
    type OperatorOccurrence MidKinded loc = Locate loc (VarRef OpName)
    type InfixedOccurrence MidKinded loc = VarRef VarName

    type ValueBinder MidKinded loc = Locate loc (Unique VarName)
    type TopValueBinder MidKinded loc = Locate loc (Qualified VarName)
    type TopTypeBinder MidKinded loc = Locate loc (Qualified TypeName)
    type TypeVariable MidKinded loc = Locate loc (Unique LowerAlphaName)
    type ConstructorBinder MidKinded loc = Locate loc (Qualified TypeName)
    type LambdaBinder MidKinded loc = TypedLambdaParam (Unique VarName) loc MidKinded

    type ExpressionMeta MidKinded loc = Maybe (Type loc MidKinded)
    type PatternMeta MidKinded loc = Maybe (Type loc MidKinded)
    type TypeMeta MidKinded loc = UniqueId

    type VariableExtension MidKinded = NoExtension
    type LambdaExtension MidKinded = NoExtension
    type LetExtension MidKinded = NoExtension
    type ApplicationExtension MidKinded = NoExtension
    type ConstructorNodeExtension MidKinded = NoExtension

    type ExpressionExtension MidKinded loc = Void
    type PatternExtension MidKinded loc = Void
    type TypeSyntaxExtension MidKinded loc = Void
    type DeclBodyExtension MidKinded loc = Void

    type ValueDeclPatterns MidKinded loc = ()
    type ValueDeclTypeAnnotation MidKinded loc = ()

    type ValueDeclMetadata MidKinded loc = Maybe (Type loc MidKinded)
    type TypeDeclMetadata MidKinded loc = NoExtension

type MidKindedType = Type SourceRegion MidKinded
type MidKindedTypeDeclaration = TypeDeclaration SourceRegion MidKinded
