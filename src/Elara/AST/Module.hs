{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Elara.AST.Module where

import Control.Lens (Each (each), toListOf, traverseOf, (^.))
import Data.Generics.Product
import Data.Generics.Wrapped
import Elara.AST.Generic (ASTLocate, ASTLocate', Declaration, FullASTQual)
import Elara.AST.Name (ModuleName, OpName, TypeName, VarName)
import Elara.AST.Region (Located, unlocated)
import Elara.Data.Pretty
import Elara.Data.Pretty.Styles qualified as Style
import Elara.Data.TopologicalGraph
import Print (debugPretty)
import Unsafe.Coerce
import Prelude hiding (Text)

newtype Module ast = Module (ASTLocate ast (Module' ast))
  deriving (Generic)

data Module' ast = Module'
  { name :: ASTLocate ast ModuleName,
    exposing :: Exposing ast,
    imports :: [Import ast],
    declarations :: [Declaration ast]
  }
  deriving (Generic)

newtype Import ast = Import (ASTLocate ast (Import' ast))
  deriving (Generic)

data Import' ast = Import'
  { importing :: ASTLocate ast ModuleName,
    as :: Maybe (ASTLocate ast ModuleName),
    qualified :: Bool,
    exposing :: Exposing ast
  }
  deriving (Generic)

data Exposing ast
  = ExposingAll
  | ExposingSome [Exposition ast]
  deriving (Generic)

data Exposition ast
  = ExposedValue (FullASTQual ast VarName) -- exposing foo
  | ExposedOp (FullASTQual ast OpName) -- exposing (+)
  | ExposedType (FullASTQual ast TypeName) -- exposing Foo
  | ExposedTypeAndAllConstructors (FullASTQual ast TypeName) -- exposing Foo(..)
  deriving (Generic)

traverseModule traverseDecl =
  traverseOf
    (_Unwrapped . unlocated)
    ( \m' -> do
        let exposing' = coerceExposing (m' ^. field' @"exposing")
        let imports' = coerceImport <$> (m' ^. field' @"imports")
        declarations' <- traverse traverseDecl (m' ^. field' @"declarations")
        pure (Module' (m' ^. field' @"name") exposing' imports' declarations')
    )

-- | "Safe" coercion between 'Exposition' types
-- Since the ASTX type families aren't injective, we can't use 'coerce' :(
coerceExposition ::
  ( FullASTQual ast1 VarName ~ FullASTQual ast2 VarName,
    FullASTQual ast1 OpName ~ FullASTQual ast2 OpName,
    FullASTQual ast1 TypeName ~ FullASTQual ast2 TypeName
  ) =>
  Exposition ast1 ->
  Exposition ast2
coerceExposition = unsafeCoerce

coerceExposing ::
  ( FullASTQual ast1 VarName ~ FullASTQual ast2 VarName,
    FullASTQual ast1 OpName ~ FullASTQual ast2 OpName,
    FullASTQual ast1 TypeName ~ FullASTQual ast2 TypeName
  ) =>
  Exposing ast1 ->
  Exposing ast2
coerceExposing = unsafeCoerce

coerceImport ::
  ( ASTLocate ast1 ModuleName ~ ASTLocate ast2 ModuleName,
    FullASTQual ast1 VarName ~ FullASTQual ast2 VarName,
    FullASTQual ast1 OpName ~ FullASTQual ast2 OpName,
    FullASTQual ast1 TypeName ~ FullASTQual ast2 TypeName
  ) =>
  Import ast1 ->
  Import ast2
coerceImport = unsafeCoerce

-- Module Graph functions
instance (ASTLocate' ast ~ Located) => HasDependencies (Module ast) where
  type Key (Module ast) = ModuleName
  key m = m ^. _Unwrapped . unlocated . field' @"name" . unlocated
  dependencies = toListOf (_Unwrapped . unlocated . field' @"imports" . each . _Unwrapped . unlocated . field' @"importing" . unlocated)

traverseModuleRevTopologically :: (_) => (Declaration ast -> f (Declaration ast')) -> Module ast1 -> f (Module ast2)
traverseModuleRevTopologically traverseDecl =
  traverseOf
    (_Unwrapped . unlocated)
    ( \m' -> do
        let exposing' = coerceExposing (m' ^. field' @"exposing")
        let imports' = coerceImport <$> (m' ^. field' @"imports")
        let declGraph = createGraph (m' ^. field' @"declarations")
        declarations' <- traverse traverseDecl (allEntriesRevTopologically declGraph)
        pure (Module' (m' ^. field' @"name") exposing' imports' declarations')
    )

instance
  ( Pretty (ASTLocate ast ModuleName),
    Pretty (ASTLocate ast (Import' ast)),
    Pretty (ASTLocate ast (Exposing ast)),
    Pretty (ASTLocate ast (Module' ast))
  ) =>
  Pretty (Module ast)
  where
  pretty (Module m) = pretty m

instance
  ( Pretty (ASTLocate ast ModuleName),
    Pretty (ASTLocate ast (Import' ast)),
    Pretty (ASTLocate ast (Exposing ast)),
    Pretty (Exposing ast),
    Pretty (Import ast),
    Pretty (Declaration ast)
  ) =>
  Pretty (Module' ast)
  where
  pretty (Module' n e i d) =
    vsep
      [ Style.keyword "module" <+> Style.moduleName (pretty n) <+> Style.keyword "exposing" <+> pretty e,
        "",
        vsep (pretty <$> i),
        "",
        vsep (pretty <$> d)
      ]

instance (Pretty (Exposition ast)) => Pretty (Exposing ast) where
  pretty ExposingAll = "(..)"
  pretty (ExposingSome e) = parens (hsep (punctuate comma (pretty <$> e)))

instance
  ( Pretty (FullASTQual ast VarName),
    Pretty (FullASTQual ast OpName),
    Pretty (FullASTQual ast TypeName)
  ) =>
  Pretty (Exposition ast)
  where
  pretty (ExposedValue n) = pretty n
  pretty (ExposedType tn) = pretty tn
  pretty (ExposedTypeAndAllConstructors tn) = pretty tn <> "(..)"
  pretty (ExposedOp o) = pretty o

instance (Pretty (ASTLocate ast (Import' ast))) => Pretty (Import ast) where
  pretty (Import i) = pretty i

instance
  ( Pretty (ASTLocate ast ModuleName),
    Pretty (Exposing ast)
  ) =>
  Pretty (Import' ast)
  where
  pretty (Import' i a q e) =
    Style.keyword "import" <+> Style.moduleName (pretty i) <> as' <> qual <+> Style.keyword "exposing" <+> pretty e
    where
      as' = case a of
        Nothing -> ""
        Just q' -> Style.keyword "as" <+> Style.moduleName (pretty q')
      qual = if q then Style.keyword "qualified" else ""

-- instance ToJSON (ASTLocate ast (Module' ast)) => ToJSON (Module ast)
-- instance
--     ( ToJSON (ASTLocate ast ModuleName)
--     , ToJSON (ASTDeclaration ast)
--     , ToJSON (Import ast)
--     , ToJSON (Exposing ast)
--     ) =>
--     ToJSON (Module' ast)

-- instance ToJSON (ASTLocate ast (Import' ast)) => ToJSON (Import ast)
-- instance
--     ( ToJSON (ASTLocate ast ModuleName)
--     , ToJSON (Exposing ast)
--     ) =>
--     ToJSON (Import' ast)

-- instance ToJSON (Exposition ast) => ToJSON (Exposing ast)

-- instance (ToJSON (FullASTQual ast VarName), ToJSON (FullASTQual ast OpName), ToJSON (FullASTQual ast TypeName)) => ToJSON (Exposition ast)
