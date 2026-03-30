{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Phases.Kinded where

import Elara.AST.Name (LowerAlphaName, OpName, Qualified, TypeName, VarName)
import Elara.AST.Phase
import Elara.AST.Region (SourceRegion)
import Elara.AST.Types
import Elara.AST.VarRef (VarRef)
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Unique (Unique)

data Kinded

instance ElaraPhase Kinded where
    type ValueOccurrence Kinded loc = Locate loc (VarRef VarName)
    type ConstructorOccurrence Kinded loc = Locate loc (Qualified TypeName)
    type TypeOccurrence Kinded loc = Locate loc (Qualified TypeName)
    type OperatorOccurrence Kinded loc = Locate loc (VarRef OpName)
    type InfixedOccurrence Kinded loc = VarRef VarName

    type ValueBinder Kinded loc = Locate loc (Unique VarName)
    type TopValueBinder Kinded loc = Locate loc (Qualified VarName)
    type TopTypeBinder Kinded loc = Locate loc (Qualified TypeName)
    type TypeVariable Kinded loc = Locate loc (Unique LowerAlphaName)
    type ConstructorBinder Kinded loc = Locate loc (Qualified TypeName)
    type LambdaBinder Kinded loc = TypedLambdaParam (Unique VarName) loc Kinded

    type ExpressionMeta Kinded loc = Maybe (Type loc Kinded)
    type PatternMeta Kinded loc = Maybe (Type loc Kinded)
    type TypeMeta Kinded loc = ElaraKind

    type VariableExtension Kinded = NoExtension
    type LambdaExtension Kinded = NoExtension
    type LetExtension Kinded = NoExtension
    type ApplicationExtension Kinded = NoExtension
    type ConstructorNodeExtension Kinded = NoExtension

    type ExpressionExtension Kinded loc = Void
    type PatternExtension Kinded loc = Void
    type TypeSyntaxExtension Kinded loc = Void
    type DeclBodyExtension Kinded loc = Void

    type ValueDeclPatterns Kinded loc = ()
    type ValueDeclTypeAnnotation Kinded loc = ()

    type ValueDeclMetadata Kinded loc = Maybe (Type loc Kinded)
    type TypeDeclMetadata Kinded loc = ElaraKind

type KindedType = Type SourceRegion Kinded
type KindedType' = Type' SourceRegion Kinded
type KindedTypeDeclaration = TypeDeclaration SourceRegion Kinded
