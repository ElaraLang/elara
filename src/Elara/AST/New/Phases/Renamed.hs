{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.New.Phases.Renamed where

import Elara.AST.Name (LowerAlphaName, OpName, Qualified, TypeName, VarName, VarOrConName)
import Elara.AST.New.Extensions
import Elara.AST.New.Phase
import Elara.AST.New.Types
import Elara.AST.Region (SourceRegion)
import Elara.AST.VarRef (VarRef)
import Elara.Data.Unique (Unique)

{- | Renamed AST stage. Key changes from Desugared:
* All names are fully qualified or uniquified
* List/Tuple expressions desugared to constructor applications
* List/Tuple/Cons patterns desugared to constructor patterns
* Binary operators and InParens still present (removed at Shunt)
-}
data Renamed

-- | Lambda binder with optional type annotation, used from Renamed onward
data TypedLambdaParam v loc p = TypedLambdaParam v (PatternMeta p loc)
    deriving (Generic)

instance ElaraPhase Renamed where
    -- Occurrences (now qualified/resolved)
    type ValueOccurrence Renamed loc = Locate loc (VarRef VarName)
    type ConstructorOccurrence Renamed loc = Locate loc (Qualified TypeName)
    type TypeOccurrence Renamed loc = Locate loc (Qualified TypeName)
    type OperatorOccurrence Renamed loc = Locate loc (VarRef OpName)
    type InfixedOccurrence Renamed loc = VarRef VarOrConName

    -- Binders (uniquified)
    type ValueBinder Renamed loc = Locate loc (Unique VarName)
    type TopValueBinder Renamed loc = Locate loc (Qualified VarName)
    type TopTypeBinder Renamed loc = Locate loc (Qualified TypeName)
    type TypeVariable Renamed loc = Locate loc (Unique LowerAlphaName)
    type ConstructorBinder Renamed loc = Locate loc (Qualified TypeName)
    type LambdaBinder Renamed loc = TypedLambdaParam (Unique VarName) loc Renamed

    -- Metadata
    type ExpressionMeta Renamed loc = Maybe (Type loc Renamed) -- carried through from source
    type PatternMeta Renamed loc = Maybe (Type loc Renamed)
    type TypeMeta Renamed loc = ()

    -- Constructor extensions
    type VariableExtension Renamed = NoExtension
    type LambdaExtension Renamed = NoExtension
    type LetExtension Renamed = NoExtension
    type ApplicationExtension Renamed = NoExtension
    type ConstructorNodeExtension Renamed = NoExtension

    -- Syntax extensions (BinOp + InParens only; List/Tuple gone)
    type ExpressionExtension Renamed loc = RenamedExpressionExtension loc
    type PatternExtension Renamed loc = Void -- list/tuple/cons patterns gone
    type TypeSyntaxExtension Renamed loc = Void -- tuple types gone
    type DeclBodyExtension Renamed loc = Void

    -- Value declaration fields (eliminated after desugar)
    type ValueDeclPatterns Renamed loc = ()
    type ValueDeclTypeAnnotation Renamed loc = ()

    -- Declaration metadata
    type ValueDeclMetadata Renamed loc = Maybe (Type loc Renamed)
    type TypeDeclMetadata Renamed loc = NoExtension

-- | Renamed-specific expression syntax (only BinOp + InParens survive)
data RenamedExpressionExtension loc
    = RenamedBinaryOperator (BinaryOperatorExtension loc Renamed)
    | RenamedInParens (InParensExtension loc Renamed)
    deriving (Generic)

-- Type aliases
type RenamedExpr = Expr SourceRegion Renamed
type RenamedExpr' = Expr' SourceRegion Renamed
type RenamedPattern = Pattern SourceRegion Renamed
type RenamedPattern' = Pattern' SourceRegion Renamed
type RenamedType = Type SourceRegion Renamed
type RenamedType' = Type' SourceRegion Renamed
type RenamedDeclaration = Declaration SourceRegion Renamed
type RenamedDeclaration' = Declaration' SourceRegion Renamed
type RenamedDeclarationBody = DeclarationBody SourceRegion Renamed
type RenamedDeclarationBody' = DeclarationBody' SourceRegion Renamed
type RenamedTypeDeclaration = TypeDeclaration SourceRegion Renamed
type RenamedBinaryOperator = BinaryOperator SourceRegion Renamed
