{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Elara.Core.Module where

import Data.Generics.Product
import Elara.AST.Name (ModuleName, Qualified)
import Elara.AST.Pretty (prettyBlockExpr)
import Elara.Core (CoreBind, DataCon, Type, TypeVariable)
import Elara.Core.Pretty (prettyTy, prettyTypeVariables)
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Pretty (AnsiStyle, Doc, Pretty (pretty), bracedBlock, hardline, indentDepth, nest, (<+>))
import Elara.Data.Pretty.Styles (keyword)
import Elara.Data.TopologicalGraph (HasDependencies (..))

data CoreModule bind = CoreModule
    { name :: !ModuleName
    , declarations :: ![CoreDeclaration bind]
    }
    deriving (Generic)

-- the constraint is necessary for 'gplate' to be able to find the fields correctly
instance bind ~ CoreBind => HasDependencies (CoreModule bind) where
    type Key (CoreModule bind) = ModuleName
    key = view (field @"name")

    dependencies m = do
        m ^.. field @"declarations" % (gplate @(Qualified Text)) % field @"qualifier"

data CoreDeclaration bind
    = CoreValue bind
    | CoreType CoreTypeDecl
    deriving (Generic)

data CoreTypeDecl = CoreTypeDecl
    { ctdName :: !(Qualified Text)
    , kind :: !ElaraKind
    , typeVars :: ![TypeVariable]
    , typeBody :: CoreTypeDeclBody
    }
    deriving (Generic)

data CoreTypeDeclBody
    = CoreTypeAlias Type
    | CoreDataDecl [DataCon]
    deriving (Generic)

instance Pretty bind => Pretty (CoreModule bind) where
    pretty (CoreModule name decls) =
        "module"
            <+> pretty name
            <> hardline
            <> nest indentDepth (bracedBlock decls)

instance Pretty bind => Pretty (CoreDeclaration bind) where
    pretty (CoreValue v) = pretty v
    pretty (CoreType t) = prettyTdef t

prettyTdef :: CoreTypeDecl -> Doc AnsiStyle
prettyTdef (CoreTypeDecl name kind tvs body) =
    keyword "type" <+> pretty name <+> prettyTypeVariables tvs <+> ":" <+> pretty kind <+> "=" <+> prettyBody body
  where
    prettyBody (CoreTypeAlias t) = prettyTy t
    prettyBody (CoreDataDecl dcs) = let ?contextFree = True in prettyBlockExpr (pretty <$> dcs)

makeFields ''CoreModule
