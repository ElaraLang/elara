{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Unlocated where

import Data.Kind qualified as Kind
import Elara.AST.Generic
import Elara.AST.Region (Located)
import Elara.AST.Select (LocatedAST (..), UnlocatedAST (..))
import Elara.AST.VarRef (UnlocatedVarRef, VarRef)
import Elara.Data.Unique (Unique)

type instance ASTLocate' 'UnlocatedFrontend = Unlocated
type instance ASTLocate' 'UnlocatedDesugared = Unlocated
type instance ASTLocate' 'UnlocatedRenamed = Unlocated
type instance ASTLocate' 'UnlocatedShunted = Unlocated
type instance ASTLocate' 'UnlocatedTyped = Unlocated

type instance Select any 'UnlocatedFrontend = Replace 'Frontend 'UnlocatedFrontend (Select any 'Frontend)
type instance Select any 'UnlocatedDesugared = Replace 'Desugared 'UnlocatedDesugared (Select any 'Desugared)
type instance Select any 'UnlocatedRenamed = Replace 'Renamed 'UnlocatedRenamed (Select any 'Renamed)
type instance Select any 'UnlocatedShunted = Replace 'Shunted 'UnlocatedShunted (Select any 'Shunted)
type instance Select any 'UnlocatedTyped = Replace 'Typed 'UnlocatedTyped (Select any 'Typed)

type family Replace (needle :: LocatedAST) (replacement :: UnlocatedAST) (haystack :: Kind.Type) where
    Replace _ _ (VarRef a) = UnlocatedVarRef a -- This doesn't really belong here but it's easier than putting it elsewhere
    Replace needle replacement (Located a) = a
    Replace needle replacement (Expr needle) = Expr replacement
    Replace needle replacement (Expr' needle) = Expr' replacement
    Replace needle replacement (Pattern needle) = Pattern replacement
    Replace needle replacement (Pattern' needle) = Pattern' replacement
    Replace needle replacement (BinaryOperator needle) = BinaryOperator replacement
    Replace needle replacement (BinaryOperator' needle) = BinaryOperator' replacement
    Replace needle replacement (Type needle) = Type replacement
    Replace needle replacement (Type' needle) = Type' replacement
    Replace needle replacement (Declaration needle) = Declaration replacement
    Replace needle replacement (Declaration' needle) = Declaration' replacement
    Replace needle replacement (DeclarationBody needle) = DeclarationBody replacement
    Replace needle replacement (DeclarationBody' needle) = DeclarationBody' replacement
    Replace needle replacement [list] = [Replace needle replacement list]
    Replace needle replacement (Maybe maybe) = Maybe (Replace needle replacement maybe)
    Replace needle replacement (Unique maybe) = Unique (Replace needle replacement maybe)
    Replace needle replacement other = other
