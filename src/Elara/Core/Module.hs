{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Elara.Core.Module where

import Data.Generics.Product
import Elara.AST.Name (ModuleName, Qualified)
import Elara.AST.Pretty (prettyBlockExpr)
import Elara.Core (CoreBind, DataCon, Type, TypeVariable)
import Elara.Core.Pretty (prettyTy, prettyTypeVariables, prettyVdefg)
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Pretty (AnsiStyle, Doc, Pretty (pretty), bracedBlock, hardline, indentDepth, nest, (<+>))
import Elara.Data.Pretty.Styles (keyword)
import Elara.Data.TopologicalGraph (HasDependencies (..))

data CoreModule = CoreModule
    { name :: !ModuleName
    , declarations :: ![CoreDeclaration]
    }
    deriving (Generic)

instance HasDependencies CoreModule where
    type Key CoreModule = ModuleName
    key = view (field @"name")

    dependencies = const [] -- TODO

data CoreDeclaration
    = CoreValue CoreBind
    | CoreType CoreTypeDecl

data CoreTypeDecl = CoreTypeDecl
    { ctdName :: !(Qualified Text)
    , kind :: !ElaraKind
    , typeVars :: ![TypeVariable]
    , typeBody :: CoreTypeDeclBody
    }

data CoreTypeDeclBody
    = CoreTypeAlias Type
    | CoreDataDecl [DataCon]

instance Pretty CoreModule where
    pretty (CoreModule name decls) =
        "module"
            <+> pretty name
            <> hardline
            <> nest indentDepth (bracedBlock decls)

instance Pretty CoreDeclaration where
    pretty (CoreValue v) = prettyVdefg v
    pretty (CoreType t) = prettyTdef t

prettyTdef :: CoreTypeDecl -> Doc AnsiStyle
prettyTdef (CoreTypeDecl name kind tvs body) =
    keyword "type" <+> pretty name <+> prettyTypeVariables tvs <+> ":" <+> pretty kind <+> "=" <+> prettyBody body
  where
    prettyBody (CoreTypeAlias t) = prettyTy t
    prettyBody (CoreDataDecl dcs) = let ?contextFree = True in prettyBlockExpr (pretty <$> dcs)

makeFields ''CoreModule
