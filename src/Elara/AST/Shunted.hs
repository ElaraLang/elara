{-# LANGUAGE UndecidableInstances #-}

{- | Shunted AST Type
 This is very similar to 'Elara.AST.Renamed.Expr' except:

 - Operators are re-shunted to match their defined precedence and associativity
 - This means there's no need for an 'InParens' token anymore so that's also gone :D
 - The confusing 'VarName'/'Elara.AST.Name.OpName' bs is also gone. Binary operator invocations are replaced with prefix function calls. This always uses VarName
-}
module Elara.AST.Shunted where

import Control.Lens (Plated, concatMapOf, cosmosOn, view, (^.), _1)
import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Wrapped
import Elara.AST.Generic (ASTLocate', ASTQual, DataConCantHappen, Expr' (..), NoFieldValue, Select)
import Elara.AST.Generic qualified as Generic
import Elara.AST.Name (LowerAlphaName, Name (..), OpName, Qualified, TypeName, VarName)
import Elara.AST.Region (Located (..), unlocated)
import Elara.AST.Select (LocatedAST (Shunted))
import Elara.AST.VarRef (VarRef, VarRef' (..))
import Elara.Data.TopologicalGraph (HasDependencies (..))
import Elara.Data.Unique (Unique)

type instance ASTLocate' 'Shunted = Located
type instance ASTQual 'Shunted = Qualified

-- Selections for 'Expr'
type instance Select "ExprType" 'Shunted = Maybe ShuntedType
type instance Select "LambdaPattern" 'Shunted = Unique VarName
type instance Select "LetPattern" 'Shunted = NoFieldValue

type instance Select "VarRef" 'Shunted = VarRef VarName
type instance Select "ConRef" 'Shunted = Qualified TypeName

type instance Select "SymOp" 'Shunted = VarRef OpName
type instance Select "Infixed" 'Shunted = VarRef VarName
type instance Select "LetParamName" 'Shunted = Unique VarName
type instance Select "InParens" 'Shunted = DataConCantHappen
type instance Select "BinaryOperator" 'Shunted = DataConCantHappen

type instance Select "PatternType" 'Shunted = Maybe ShuntedType
type instance Select "VarPat" 'Shunted = Unique LowerAlphaName
type instance Select "ConPat" 'Shunted = Qualified TypeName

-- Selections for 'DeclarationBody'
type instance Select "ValuePatterns" 'Shunted = NoFieldValue
type instance Select "ValueType" 'Shunted = Maybe ShuntedType
type instance Select "ValueTypeDef" 'Shunted = DataConCantHappen

-- Selections for 'Declaration'
type instance Select "DeclarationName" 'Shunted = Qualified Name

-- Selections for 'Type'
type instance Select "TypeVar" 'Shunted = Unique LowerAlphaName
type instance Select "UserDefinedType" 'Shunted = Qualified TypeName
type instance Select "ConstructorName" 'Shunted = Qualified TypeName

type ShuntedExpr = Generic.Expr 'Shunted
type ShuntedExpr' = Generic.Expr' 'Shunted

type ShuntedPattern = Generic.Pattern 'Shunted
type ShuntedPattern' = Generic.Pattern' 'Shunted

type ShuntedBinaryOperator = Generic.BinaryOperator 'Shunted
type ShuntedBinaryOperator' = Generic.BinaryOperator' 'Shunted

type ShuntedType = Generic.Type 'Shunted
type ShuntedType' = Generic.Type' 'Shunted

type ShuntedDeclaration = Generic.Declaration 'Shunted
type ShuntedDeclaration' = Generic.Declaration' 'Shunted

type ShuntedDeclarationBody = Generic.DeclarationBody 'Shunted
type ShuntedDeclarationBody' = Generic.DeclarationBody' 'Shunted

type ShuntedTypeDeclaration = Generic.TypeDeclaration 'Shunted

instance Plated ShuntedExpr
instance Plated ShuntedExpr'
instance Plated ShuntedType
instance Plated ShuntedType'

deriving instance Data ShuntedExpr'
deriving instance Data ShuntedExpr
deriving instance Data ShuntedBinaryOperator
deriving instance Data ShuntedBinaryOperator'
deriving instance Data ShuntedType
deriving instance Data ShuntedType'
deriving instance Data ShuntedPattern
deriving instance Data ShuntedPattern'

instance HasDependencies ShuntedDeclaration where
    type Key ShuntedDeclaration = Qualified Name
    key = view (_Unwrapped . unlocated . field' @"name" . unlocated)
    dependencies decl = case decl ^. _Unwrapped . unlocated . field' @"body" . _Unwrapped . unlocated of
        Generic.Value e _ _ -> valueDependencies e
        Generic.TypeDeclaration _ _ -> []

valueDependencies :: ShuntedExpr -> [Qualified Name]
valueDependencies =
    concatMapOf (cosmosOn (_Unwrapped . _1 . unlocated)) names
  where
    names :: ShuntedExpr' -> [Qualified Name]
    names (Var (Located _ (Global e))) = NVarName <<$>> [e ^. unlocated]
    names (Constructor (Located _ e)) = [NTypeName <$> e]
    names _ = []
