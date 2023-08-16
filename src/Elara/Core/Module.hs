{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Elara.Core.Module where

import Control.Lens (makeFields, view)
import Elara.AST.Name (ModuleName)
import Elara.Core (CoreBind)
import Elara.Core.Pretty (prettyVdefg)
import Elara.Data.Pretty (Pretty (pretty), bracedBlock, hardline, indentDepth, nest, (<+>))
import Elara.Data.TopologicalGraph (HasDependencies (..))
import Data.Generics.Product

data CoreModule = CoreModule
    { name :: ModuleName
    , declarations :: [CoreDeclaration]
    }
    deriving Generic

instance HasDependencies CoreModule where
    type Key CoreModule = ModuleName
    key = view (field @"name")

    dependencies = const [] -- TODO

newtype CoreDeclaration
    = CoreValue CoreBind

instance Pretty CoreModule where
    pretty (CoreModule name decls) =
        "module"
            <+> pretty name
                <> hardline
                <> nest indentDepth (bracedBlock decls)

instance Pretty CoreDeclaration where
    pretty (CoreValue v) = prettyVdefg v

makeFields ''CoreModule
