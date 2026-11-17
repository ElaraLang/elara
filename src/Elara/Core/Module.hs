{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Elara.Core.Module where

import Elara.AST.Name (ModuleName, Qualified)
import Elara.Core (DataCon, Type, TypeVariable)
import Elara.Core.Pretty (prettyTy, prettyTypeVariables)
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Pretty (AnsiStyle, Doc, Pretty (pretty), bracedBlock, hardline, indentDepth, nest, (<+>))
import Elara.Data.Pretty.Styles (keyword)
import Elara.Pretty.Common (prettyCtorsInline)

import Elara.Core qualified as Core

data CoreModule bind = CoreModule
    { name :: !ModuleName
    , declarations :: ![CoreDeclaration bind]
    }
    deriving (Generic)

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
    | CoreDataDecl Core.TyCon [DataCon]
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

instance Pretty CoreTypeDecl where
    pretty = prettyTdef

instance Pretty CoreTypeDeclBody where
    pretty (CoreTypeAlias t) = prettyTy t
    pretty (CoreDataDecl (Core.TyCon _ (Core.Prim _)) _) = "<primitive>"
    pretty (CoreDataDecl _ []) = "{}"
    pretty (CoreDataDecl _ dcs) = prettyCtorsInline (pretty <$> dcs)

prettyTdef :: CoreTypeDecl -> Doc AnsiStyle
prettyTdef (CoreTypeDecl name kind tvs body) =
    keyword "type" <+> pretty name <> prettyTypeVariables tvs <+> ":" <+> pretty kind <+> "=" <+> pretty body

makeFields ''CoreModule
