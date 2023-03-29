{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Select where

import Control.Lens (Getting, Lens', view)
import Elara.AST.Desugared qualified as Desugared
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Frontend.Unlocated qualified as Frontend.Unlocated
import Elara.AST.Name (MaybeQualified, ModuleName, Name, Qualified)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Renamed qualified as Renamed

data Frontend

data UnlocatedFrontend

data Desugared

data Renamed

type family ASTExpr ast where
    ASTExpr Frontend = Frontend.Expr
    ASTExpr UnlocatedFrontend = Frontend.Unlocated.Expr
    ASTExpr Desugared = Desugared.Expr
    ASTExpr Renamed = Renamed.Expr

type family ASTType ast where
    ASTType Frontend = Frontend.Type
    ASTType UnlocatedFrontend = Frontend.Unlocated.Type
    ASTType Desugared = Desugared.Type
    ASTType Renamed = Renamed.Type

type family ASTPattern ast where
    ASTPattern Frontend = Frontend.Pattern
    ASTPattern UnlocatedFrontend = Frontend.Unlocated.Pattern
    ASTPattern Desugared = Desugared.Pattern
    ASTPattern Renamed = Renamed.Pattern

type family ASTQual ast where
    ASTQual Frontend = MaybeQualified
    ASTQual UnlocatedFrontend = MaybeQualified
    ASTQual Desugared = MaybeQualified
    ASTQual Renamed = Qualified

type family ASTLocate' ast where
    ASTLocate' Frontend = Located
    ASTLocate' UnlocatedFrontend = Unlocated
    ASTLocate' Desugared = Located
    ASTLocate' Renamed = Located

type family ASTDeclaration ast where
    ASTDeclaration Frontend = Frontend.Declaration
    ASTDeclaration UnlocatedFrontend = Frontend.Unlocated.Declaration
    ASTDeclaration Desugared = Desugared.Declaration
    ASTDeclaration Renamed = Renamed.Declaration

type ASTLocate ast a = UnwrapUnlocated (ASTLocate' ast a)

newtype Unlocated a = Unlocated a

-- | Unwraps a single layer of @Unlocated@ from a type.
type family UnwrapUnlocated g where
    UnwrapUnlocated (Unlocated a) = a
    UnwrapUnlocated a = a

type FullASTQual ast a = ((ASTLocate ast) (ASTQual ast a))

type family Unlocate g where
    Unlocate (Located a) = a
    Unlocate a = a

class GetLocation ast where
    getLocation :: forall a. FullASTQual ast a -> Maybe SourceRegion
    getLocation' :: ASTLocate ast a -> Maybe SourceRegion

class RUnlocate ast where
    rUnlocate :: FullASTQual ast a -> ASTQual ast a
    rUnlocate' :: ASTLocate ast a -> a
    rUnlocated :: Lens' (FullASTQual ast a) (ASTQual ast a)
    rUnlocated' :: Lens' (ASTLocate ast a) a
    fmapRUnlocate :: (a -> b) -> FullASTQual ast a -> FullASTQual ast b
    fmapRUnlocate' :: (a -> b) -> ASTLocate ast a -> ASTLocate ast b

    sequenceRUnlocate' :: Functor f => ASTLocate ast (f a) -> f (ASTLocate ast a)

rUnlocateVia ::
    forall ast a s.
    RUnlocate ast =>
    Getting (UnwrapUnlocated (ASTLocate' ast (ASTQual ast a))) s (UnwrapUnlocated (ASTLocate' ast (ASTQual ast a))) ->
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
    rUnlocated = unlocated
    rUnlocated' = unlocated
    fmapRUnlocate f (Located r a) = Located r (fmap f a)
    fmapRUnlocate' f (Located r a) = Located r (f a)
    sequenceRUnlocate' :: Functor f => Located (f a) -> f (Located a)
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs

instance GetLocation Frontend where
    getLocation (Located r _) = Just r
    getLocation' (Located r _) = Just r

instance RUnlocate Desugared where
    rUnlocate (Located _ a) = a
    rUnlocate' (Located _ a) = a
    rUnlocated = unlocated
    rUnlocated' = unlocated
    fmapRUnlocate f (Located r a) = Located r (fmap f a)
    fmapRUnlocate' f (Located r a) = Located r (f a)
    sequenceRUnlocate' :: Functor f => Located (f a) -> f (Located a)
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs

instance RUnlocate Renamed where
    rUnlocate (Located _ a) = a
    rUnlocate' (Located _ a) = a
    rUnlocated = unlocated
    rUnlocated' = unlocated
    fmapRUnlocate f (Located r a) = Located r (fmap f a)
    fmapRUnlocate' f (Located r a) = Located r (f a)
    sequenceRUnlocate' :: Functor f => Located (f a) -> f (Located a)
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs

instance GetLocation Renamed where
    getLocation (Located r _) = Just r
    getLocation' (Located r _) = Just r

instance RUnlocate UnlocatedFrontend where
    rUnlocate a = a
    rUnlocate' a = a
    rUnlocated = id
    rUnlocated' = id
    fmapRUnlocate = fmap
    fmapRUnlocate' = id
    sequenceRUnlocate' = id

instance GetLocation UnlocatedFrontend where
    getLocation _ = Nothing
    getLocation' _ = Nothing

class HasModuleName c ast | c -> ast where
    moduleName :: Lens' c (ASTLocate ast ModuleName)
    unlocatedModuleName :: Lens' c ModuleName

class HasName a b | a -> b where
    name :: Lens' a b

class HasDeclarationName c ast where
    declarationName :: Lens' c (ASTLocate ast Name)
    unlocatedDeclarationName :: Lens' c Name

instance HasDeclarationName Frontend.Declaration Frontend where
    declarationName = Frontend._Declaration . unlocated . Frontend.declaration'Name
    unlocatedDeclarationName = declarationName @Frontend.Declaration @Frontend . unlocated

instance HasModuleName Frontend.Declaration Frontend where
    moduleName = Frontend._Declaration . unlocated . Frontend.declaration'Module'
    unlocatedModuleName = moduleName @Frontend.Declaration @Frontend . unlocated

instance HasName Frontend.Declaration (Located Name) where
    name = Frontend._Declaration . unlocated . Frontend.declaration'Name

instance HasName Frontend.Declaration' (Located Name) where
    name = Frontend.declaration'Name

instance HasModuleName Desugared.Declaration Desugared where
    moduleName = Desugared._Declaration . unlocated . moduleName @Desugared.Declaration' @Desugared
    unlocatedModuleName :: Lens' Desugared.Declaration ModuleName
    unlocatedModuleName = moduleName @Desugared.Declaration @Desugared . unlocated

instance HasModuleName Desugared.Declaration' Desugared where
    moduleName = Desugared.declaration'Module'
    unlocatedModuleName = moduleName @Desugared.Declaration' @Desugared . unlocated

instance HasName Desugared.Declaration (Located Name) where
    name = Desugared._Declaration . unlocated . Desugared.declaration'Name

instance HasName Desugared.Declaration' (Located Name) where
    name = Desugared.declaration'Name

