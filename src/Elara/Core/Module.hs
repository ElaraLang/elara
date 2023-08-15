{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Elara.Core.Module where

import Control.Lens (makeFields)
import Elara.AST.Name (ModuleName)
import Elara.Core (CoreBind)
import Elara.Core.Pretty (prettyVdefg)
import Elara.Data.Pretty (Pretty (pretty), bracedBlock, hardline, indentDepth, nest, (<+>))
import Elara.Data.TopologicalGraph (HasDependencies (..))

data CoreModule = CoreModule
    { coreModuleName :: ModuleName
    , coreModuleDeclarations :: [CoreDeclaration]
    }

instance HasDependencies CoreModule where
    type Key CoreModule = ModuleName
    key = coreModuleName

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
