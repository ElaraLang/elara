{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Phases.Shunted where

import Data.Data (Data)
import Elara.AST.Name (LowerAlphaName, OpName, Qualified, TypeName, VarName)
import Elara.AST.New.Phase
import Elara.AST.New.Phases.Renamed (TypedLambdaParam)
import Elara.AST.New.Types
import Elara.AST.Region (SourceRegion)
import Elara.AST.VarRef (VarRef)
import Elara.Data.Unique (Unique)

data Shunted deriving (Show, Eq, Ord, Generic)

instance ElaraPhase Shunted where
    type ValueOccurrence Shunted loc = Locate loc (VarRef VarName)
    type ConstructorOccurrence Shunted loc = Locate loc (Qualified TypeName)
    type TypeOccurrence Shunted loc = Locate loc (Qualified TypeName)
    type OperatorOccurrence Shunted loc = Locate loc (VarRef OpName)
    type InfixedOccurrence Shunted loc = VarRef VarName

    type ValueBinder Shunted loc = Locate loc (Unique VarName)
    type TopValueBinder Shunted loc = Locate loc (Qualified VarName)
    type TopTypeBinder Shunted loc = Locate loc (Qualified TypeName)
    type TypeVariable Shunted loc = Locate loc (Unique LowerAlphaName)
    type ConstructorBinder Shunted loc = Locate loc (Qualified TypeName)
    type LambdaBinder Shunted loc = TypedLambdaParam (Unique VarName) loc Shunted

    type ExpressionMeta Shunted loc = Maybe (Type loc Shunted)
    type PatternMeta Shunted loc = Maybe (Type loc Shunted)
    type TypeMeta Shunted loc = ()

    type VariableExtension Shunted = NoExtension
    type LambdaExtension Shunted = NoExtension
    type LetExtension Shunted = NoExtension
    type ApplicationExtension Shunted = NoExtension
    type ConstructorNodeExtension Shunted = NoExtension

    type ExpressionExtension Shunted loc = Void
    type PatternExtension Shunted loc = Void
    type TypeSyntaxExtension Shunted loc = Void
    type DeclBodyExtension Shunted loc = Void

    type ValueDeclMetadata Shunted loc = Maybe (Type loc Shunted)
    type TypeDeclMetadata Shunted loc = NoExtension

-- Type aliases
type ShuntedExpr = Expr SourceRegion Shunted
type ShuntedExpr' = Expr' SourceRegion Shunted
type ShuntedPattern = Pattern SourceRegion Shunted
type ShuntedPattern' = Pattern' SourceRegion Shunted
type ShuntedType = Type SourceRegion Shunted
type ShuntedDeclaration = Declaration SourceRegion Shunted
type ShuntedTypeDeclaration = TypeDeclaration SourceRegion Shunted
