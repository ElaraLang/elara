{-# LANGUAGE QuantifiedConstraints #-}
module Elara.AST.Select where

import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Frontend.Unlocated qualified as Frontend.Unlocated
import Elara.AST.Name (MaybeQualified, Qualified)
import Elara.AST.Region (Located (Located))
import Prelude hiding (Compose)
data Frontend

data UnlocatedFrontend

data Annotated

type family ASTExpr ast where
    ASTExpr Frontend = Frontend.Expr
    ASTExpr UnlocatedFrontend = Frontend.Unlocated.Expr
    ASTExpr Annotated = Annotated.Expr

type family ASTPattern ast where
    ASTPattern Frontend = Frontend.Pattern
    ASTPattern UnlocatedFrontend = Frontend.Pattern
    ASTPattern Annotated = Annotated.Pattern

type family ASTAnnotation ast where
    ASTAnnotation Frontend = Maybe Frontend.TypeAnnotation
    ASTAnnotation UnlocatedFrontend = Maybe Frontend.TypeAnnotation
    ASTAnnotation Annotated = Maybe Annotated.TypeAnnotation

type family ASTQual ast where
    ASTQual Frontend = MaybeQualified
    ASTQual UnlocatedFrontend = MaybeQualified
    ASTQual Annotated = Qualified

type family ASTLocate ast where
    ASTLocate Frontend = Located
    ASTLocate UnlocatedFrontend = Unlocated
    ASTLocate Annotated = Located

newtype Unlocated a = Unlocated a

type family UnwrapUnlocated g where
    UnwrapUnlocated (Unlocated a) = a
    UnwrapUnlocated a = a


type FullASTQual ast a = UnwrapUnlocated ((ASTLocate ast) (ASTQual ast a))


class Functor f => CoApplicative f where
    extract :: f a -> a

instance CoApplicative (Located) where
    extract (Located _ a) = a

-- rUnlocate Located (MaybeQualified a) = MaybeQualified a 
-- fmapRUnlocate :: (a -> b) -> Located (MaybeQualified a) -> Located (MaybeQualified b)
-- rUnlocate MaybeQualified a = MaybeQualified b
-- fmapRUnlocate :: (a -> b) -> MaybeQualified a -> MaybeQualified b

type family Unlocate g where
    Unlocate (Located a) = a
    Unlocate a = a

class RUnlocate f where
    rUnlocate :: f a -> Unlocate (f a)

instance RUnlocate Located where
    rUnlocate (Located _ a) = a

instance (forall a. Unlocate (f a) ~ f a) => RUnlocate f where
    rUnlocate f = f
