{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Phases.MidKinded where

import Elara.AST.Location
import Elara.AST.Name (LowerAlphaName, OpName, Qualified, TypeName, VarName)
import Elara.AST.Phase
import Elara.AST.Region (SourceRegion)
import Elara.AST.Types
import Elara.AST.VarRef (VarRef)
import Elara.Data.Unique (Unique, UniqueId)

data MidKinded

instance ElaraPhase MidKinded where
    type ValueOccurrence MidKinded loc = LocateNode VarNode loc (VarRef VarName)
    type ConstructorOccurrence MidKinded loc = LocateNode TypeNode loc (Qualified TypeName)
    type TypeOccurrence MidKinded loc = LocateNode TypeNode loc (Qualified TypeName)
    type OperatorOccurrence MidKinded loc = LocateNode VarNode loc (VarRef OpName)
    type InfixedOccurrence MidKinded loc = VarRef VarName

    type ValueBinder MidKinded loc = LocateNode VarNode loc (Unique VarName)
    type TopValueBinder MidKinded loc = LocateNode VarNode loc (Qualified VarName)
    type TopTypeBinder MidKinded loc = LocateNode TypeNode loc (Qualified TypeName)
    type TypeVariable MidKinded loc = LocateNode TypeNode loc (Unique LowerAlphaName)
    type ConstructorBinder MidKinded loc = LocateNode TypeNode loc (Qualified TypeName)
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
