module Elara.Core.Module where

import Elara.AST.Pretty (prettyBlockExpr)
import Elara.Core (CoreExpr, Expr)
import Elara.Data.Pretty (Pretty (pretty), bracedBlock, hardline, indentDepth, nest, (<+>))
import Elara.Data.TopologicalGraph (HasDependencies (..))

data CoreModule = CoreModule
    { coreModuleName :: Text
    , coreModuleDeclarations :: [CoreDeclaration]
    }

instance HasDependencies CoreModule where
    type Key CoreModule = Text
    key = coreModuleName

    dependencies = const [] -- TODO

data CoreDeclaration = CoreDeclaration
    { coreDeclarationName :: Text
    , coreDeclarationBody :: CoreDeclarationBody
    }

newtype CoreDeclarationBody
    = CoreValue CoreExpr

instance Pretty CoreModule where
    pretty (CoreModule name decls) =
        "module"
            <+> pretty name
                <> hardline
                <> nest indentDepth (bracedBlock decls)

instance Pretty CoreDeclaration where
    pretty (CoreDeclaration name body) =
        pretty name
            <> " = "
            <> pretty body

instance Pretty CoreDeclarationBody where
    pretty (CoreValue e) = pretty e
