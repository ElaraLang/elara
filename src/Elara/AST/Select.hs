{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.AST.Select where

import Control.Lens (Getting, Lens', view)
import Elara.AST.Desugared qualified as Desugared
import Elara.AST.Frontend qualified as Frontend
import Elara.AST.Name (MaybeQualified, ModuleName, Name, Qualified)
import Elara.AST.Region (Located (Located), SourceRegion, unlocated)
import Elara.AST.Renamed qualified as Renamed
import Elara.AST.Shunted qualified as Shunted
import Elara.AST.Typed qualified as Typed
import Elara.AST.Unlocated.Frontend qualified as Unlocated.Frontend

data Frontend

data UnlocatedFrontend

data Desugared

data Renamed

data Shunted

data PartialTyped

data Typed

type family ASTExpr ast where
    ASTExpr Frontend = Frontend.Expr
    ASTExpr UnlocatedFrontend = Unlocated.Frontend.Expr
    ASTExpr Desugared = Desugared.Expr
    ASTExpr Renamed = Renamed.Expr
    ASTExpr Shunted = Shunted.Expr
    ASTExpr PartialTyped = Typed.Expr Typed.PartialType
    ASTExpr Typed = Typed.Expr Typed.Type

type family ASTType ast where
    ASTType Frontend = Frontend.Type
    ASTType UnlocatedFrontend = Unlocated.Frontend.Type
    ASTType Desugared = Desugared.Type
    ASTType Renamed = Renamed.Type
    ASTType Shunted = Shunted.Type
    ASTType PartialTyped = Typed.PartialType
    ASTType Typed = Typed.Type

type family ASTPattern ast where
    ASTPattern Frontend = Frontend.Pattern
    ASTPattern UnlocatedFrontend = Unlocated.Frontend.Pattern
    ASTPattern Desugared = Desugared.Pattern
    ASTPattern Renamed = Renamed.Pattern
    ASTPattern Shunted = Shunted.Pattern
    ASTPattern PartialTyped = Typed.Pattern Typed.PartialType
    ASTPattern Typed = Typed.Pattern Typed.Type

type family ASTQual ast where
    ASTQual Frontend = MaybeQualified
    ASTQual UnlocatedFrontend = MaybeQualified
    ASTQual Desugared = MaybeQualified
    ASTQual Renamed = Qualified
    ASTQual Shunted = Qualified
    ASTQual PartialTyped = Qualified
    ASTQual Typed = Qualified

type family ASTLocate' ast where
    ASTLocate' Frontend = Located
    ASTLocate' UnlocatedFrontend = Unlocated
    ASTLocate' Desugared = Located
    ASTLocate' Renamed = Located
    ASTLocate' Shunted = Located
    ASTLocate' PartialTyped = Located
    ASTLocate' Typed = Located

type family ASTDeclaration ast where
    ASTDeclaration Frontend = Frontend.Declaration
    ASTDeclaration UnlocatedFrontend = Unlocated.Frontend.Declaration
    ASTDeclaration Desugared = Desugared.Declaration
    ASTDeclaration Renamed = Renamed.Declaration
    ASTDeclaration Shunted = Shunted.Declaration
    ASTDeclaration PartialTyped = Typed.Declaration Typed.PartialType
    ASTDeclaration Typed = Typed.Declaration Typed.Type

type ASTLocate ast a = UnwrapUnlocated (ASTLocate' ast a)

newtype Unlocated a = Unlocated a

-- | Unwraps a single layer of 'Unlocated' from a type.
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

    sequenceRUnlocate' :: (Functor f) => ASTLocate ast (f a) -> f (ASTLocate ast a)

rUnlocateVia ::
    forall ast a s.
    (RUnlocate ast) =>
    Getting (UnwrapUnlocated (ASTLocate' ast (ASTQual ast a))) s (UnwrapUnlocated (ASTLocate' ast (ASTQual ast a))) ->
    s ->
    ASTQual ast a
rUnlocateVia f = rUnlocate @ast . view f

rUnlocateVia' ::
    forall ast s c.
    (RUnlocate ast) =>
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
    sequenceRUnlocate' :: (Functor f) => Located (f a) -> f (Located a)
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
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs

instance RUnlocate Renamed where
    rUnlocate (Located _ a) = a
    rUnlocate' (Located _ a) = a
    rUnlocated = unlocated
    rUnlocated' = unlocated
    fmapRUnlocate f (Located r a) = Located r (fmap f a)
    fmapRUnlocate' f (Located r a) = Located r (f a)
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs

instance GetLocation Renamed where
    getLocation (Located r _) = Just r
    getLocation' (Located r _) = Just r

instance RUnlocate UnlocatedFrontend where
    rUnlocate a = a
    rUnlocate' a = a
    rUnlocated = identity
    rUnlocated' = identity
    fmapRUnlocate = fmap
    fmapRUnlocate' = identity
    sequenceRUnlocate' = identity

instance GetLocation UnlocatedFrontend where
    getLocation _ = Nothing
    getLocation' _ = Nothing

instance RUnlocate Typed where
    rUnlocate (Located _ a) = a
    rUnlocate' (Located _ a) = a
    rUnlocated = unlocated
    rUnlocated' = unlocated
    fmapRUnlocate f (Located r a) = Located r (fmap f a)
    fmapRUnlocate' f (Located r a) = Located r (f a)
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs


instance RUnlocate PartialTyped where
    rUnlocate (Located _ a) = a
    rUnlocate' (Located _ a) = a
    rUnlocated = unlocated
    rUnlocated' = unlocated
    fmapRUnlocate f (Located r a) = Located r (fmap f a)
    fmapRUnlocate' f (Located r a) = Located r (f a)
    sequenceRUnlocate' (Located r fs) = fmap (Located r) fs

class HasModuleName c ast | c -> ast where
    moduleName :: Lens' c (ASTLocate ast ModuleName)
    unlocatedModuleName :: Lens' c ModuleName

instance HasModuleName Desugared.Declaration Desugared where
    moduleName = Desugared._Declaration . unlocated . moduleName @Desugared.Declaration' @Desugared
    unlocatedModuleName :: Lens' Desugared.Declaration ModuleName
    unlocatedModuleName = moduleName @Desugared.Declaration @Desugared . unlocated

instance HasModuleName Desugared.Declaration' Desugared where
    moduleName = Desugared.declaration'Module'
    unlocatedModuleName = moduleName @Desugared.Declaration' @Desugared . unlocated

instance HasModuleName Renamed.Declaration' Renamed where
    moduleName = Renamed.declaration'Module'
    unlocatedModuleName = moduleName @Renamed.Declaration' @Renamed . unlocated

instance HasModuleName Shunted.Declaration Shunted where
    moduleName = Shunted._Declaration . unlocated . moduleName @Shunted.Declaration' @Shunted
    unlocatedModuleName :: Lens' Shunted.Declaration ModuleName
    unlocatedModuleName = moduleName @Shunted.Declaration @Shunted . unlocated

instance HasModuleName Shunted.Declaration' Shunted where
    moduleName = Shunted.declaration'Module'
    unlocatedModuleName = moduleName @Shunted.Declaration' @Shunted . unlocated

instance HasModuleName Frontend.Declaration Frontend where
    moduleName = Frontend._Declaration . unlocated . Frontend.declaration'Module'
    unlocatedModuleName = moduleName @Frontend.Declaration @Frontend . unlocated

instance HasModuleName Renamed.Declaration Renamed where
    moduleName = Renamed._Declaration . unlocated . moduleName @Renamed.Declaration' @Renamed
    unlocatedModuleName :: Lens' Renamed.Declaration ModuleName
    unlocatedModuleName = moduleName @Renamed.Declaration @Renamed . unlocated

instance HasModuleName (Typed.Declaration' Typed.PartialType) PartialTyped where
    moduleName = Typed.declaration'Module'
    unlocatedModuleName = moduleName @(Typed.Declaration' Typed.PartialType) @PartialTyped . unlocated

instance HasModuleName (Typed.Declaration' Typed.Type) Typed where
    moduleName = Typed.declaration'Module'
    unlocatedModuleName = moduleName @(Typed.Declaration' Typed.Type) @Typed . unlocated

class HasName a b | a -> b where
    name :: Lens' a b

class HasDeclarationName c ast where
    declarationName :: Lens' c (ASTLocate ast Name)
    unlocatedDeclarationName :: Lens' c Name

instance HasDeclarationName Frontend.Declaration Frontend where
    declarationName = Frontend._Declaration . unlocated . Frontend.declaration'Name
    unlocatedDeclarationName = declarationName @Frontend.Declaration @Frontend . unlocated

instance HasName Frontend.Declaration (Located Name) where
    name = Frontend._Declaration . unlocated . Frontend.declaration'Name

instance HasName Frontend.Declaration' (Located Name) where
    name = Frontend.declaration'Name

instance HasName Desugared.Declaration (Located Name) where
    name = Desugared._Declaration . unlocated . Desugared.declaration'Name

instance HasName Desugared.Declaration' (Located Name) where
    name = Desugared.declaration'Name

instance HasName Renamed.Declaration (Located (Qualified Name)) where
    name = Renamed._Declaration . unlocated . name

instance HasName Renamed.Declaration' (Located (Qualified Name)) where
    name = Renamed.declaration'Name

instance HasName Shunted.Declaration (Located (Qualified Name)) where
    name = Shunted._Declaration . unlocated . name

instance HasName Shunted.Declaration' (Located (Qualified Name)) where
    name = Shunted.declaration'Name

instance HasName (Typed.Declaration' Typed.PartialType) (Located (Qualified Name)) where
    name = Typed.declaration'Name

instance HasName (Typed.Declaration' Typed.Type) (Located (Qualified Name)) where
    name = Typed.declaration'Name

instance HasName (Typed.Declaration Typed.Type) (Located (Qualified Name)) where
    name = Typed._Declaration . unlocated . name

instance HasName (Typed.Declaration Typed.PartialType) (Located (Qualified Name)) where
    name = Typed._Declaration . unlocated . name