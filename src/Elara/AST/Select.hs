{-# LANGUAGE AllowAmbiguousTypes #-}

module Elara.AST.Select where

import Control.Lens (Getting, view)
import Elara.AST.Annotated qualified as Annotated
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Frontend.Unlocated qualified as Frontend.Unlocated
import Elara.AST.Name (MaybeQualified, Qualified)
import Elara.AST.Region (Located (Located), SourceRegion)

data Frontend

data UnlocatedFrontend

data Annotated

type family ASTExpr ast where
    ASTExpr Frontend = Frontend.Expr
    ASTExpr UnlocatedFrontend = Frontend.Unlocated.Expr
    ASTExpr Annotated = Annotated.Expr

type family ASTType ast where
    ASTType Frontend = Frontend.Type
    ASTType UnlocatedFrontend = Frontend.Unlocated.Type
    ASTType Annotated = Annotated.Type

type family ASTPattern ast where
    ASTPattern Frontend = Frontend.Pattern
    ASTPattern UnlocatedFrontend = Frontend.Unlocated.Pattern
    ASTPattern Annotated = Annotated.Pattern

type family ASTAnnotation ast where
    ASTAnnotation Frontend = Maybe Frontend.TypeAnnotation
    ASTAnnotation UnlocatedFrontend = Maybe Frontend.Unlocated.TypeAnnotation
    ASTAnnotation Annotated = Maybe Annotated.TypeAnnotation

type family ASTQual ast where
    ASTQual Frontend = MaybeQualified
    ASTQual UnlocatedFrontend = MaybeQualified
    ASTQual Annotated = Qualified

type family ASTLocate' ast where
    ASTLocate' Frontend = Located
    ASTLocate' UnlocatedFrontend = Unlocated
    ASTLocate' Annotated = Located

type ASTLocate ast a = UnwrapUnlocated (ASTLocate' ast a)

newtype Unlocated a = Unlocated a

type family UnwrapUnlocated g where
    UnwrapUnlocated (Unlocated a) = a
    UnwrapUnlocated a = a

type FullASTQual ast a = UnwrapUnlocated ((ASTLocate ast) (ASTQual ast a))

type family Unlocate g where
    Unlocate (Located a) = a
    Unlocate a = a

class GetLocation ast where
    getLocation :: forall a. FullASTQual ast a -> Maybe SourceRegion
    getLocation' :: ASTLocate ast a -> Maybe SourceRegion

class RUnlocate ast where
    rUnlocate :: FullASTQual ast a -> ASTQual ast a
    rUnlocate' :: ASTLocate ast a -> a
    fmapRUnlocate :: (a -> b) -> FullASTQual ast a -> FullASTQual ast b
    fmapRUnlocate' :: (a -> b) -> ASTLocate ast a -> ASTLocate ast b

    sequenceRUnlocate' :: Functor f => ASTLocate ast (f a) -> f (ASTLocate ast a)

rUnlocateVia ::
    forall ast a s.
    RUnlocate ast =>
    Getting (UnwrapUnlocated (UnwrapUnlocated (ASTLocate' ast (ASTQual ast a)))) s (UnwrapUnlocated (UnwrapUnlocated (ASTLocate' ast (ASTQual ast a)))) ->
    s ->
    ASTQual ast a
rUnlocateVia f = rUnlocate @ast . view f

rUnlocateVia' ::
    forall ast s c.
    RUnlocate ast =>
    Getting (UnwrapUnlocated (ASTLocate' ast c)) s (UnwrapUnlocated (ASTLocate' ast c)) ->
    s ->
    c
rUnlocateVia' f = rUnlocate' @ast . view f

instance RUnlocate Frontend where
    rUnlocate (Located _ a) = a
    rUnlocate' (Located _ a) = a
    fmapRUnlocate f (Located r a) = Located r (fmap f a)
    fmapRUnlocate' f (Located r a) = Located r (f a)
    sequenceRUnlocate' :: Functor f => Located (f a) -> f (Located a)
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs

instance GetLocation Frontend where
    getLocation (Located r _) = Just r
    getLocation' (Located r _) = Just r

instance RUnlocate Annotated where
    rUnlocate (Located _ a) = a
    rUnlocate' (Located _ a) = a
    fmapRUnlocate f (Located r a) = Located r (fmap f a)
    fmapRUnlocate' f (Located r a) = Located r (f a)
    sequenceRUnlocate' :: Functor f => Located (f a) -> f (Located a)
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs

instance GetLocation Annotated where
    getLocation (Located r _) = Just r
    getLocation' (Located r _) = Just r

instance RUnlocate UnlocatedFrontend where
    rUnlocate a = a
    rUnlocate' a = a
    fmapRUnlocate = fmap
    fmapRUnlocate' = id
    sequenceRUnlocate' = id

instance GetLocation UnlocatedFrontend where
    getLocation _ = Nothing
    getLocation' _ = Nothing