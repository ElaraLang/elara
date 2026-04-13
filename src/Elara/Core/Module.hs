{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Elara.Core.Module where

import Data.Generics.Product
import Elara.AST.Name (ModuleName, Qualified)
import Elara.Core (CoreBind, DataCon, Type, TypeVariable)
import Elara.Core qualified as Core
import Elara.Core.ANF qualified as ANF
import Elara.Core.Pretty (prettyTy, prettyTypeVariables)
import Elara.Data.Kind (ElaraKind)
import Elara.Data.Pretty (AnsiStyle, Doc, Pretty (pretty), bracedBlock, hardline, indentDepth, nest, (<+>))
import Elara.Data.Pretty.Styles (keyword)
import Elara.Data.TopologicalGraph (HasDependencies (..))
import Elara.Pretty.Common (prettyCtorsInline)

data CoreModule bind = CoreModule
    { name :: !ModuleName
    , declarations :: ![CoreDeclaration bind]
    }
    deriving (Generic)

-- the constraint is necessary for 'gplate' to be able to find the fields correctly
instance HasDependencies (CoreModule CoreBind) where
    type Key (CoreModule CoreBind) = ModuleName
    key = view (field @"name")

    dependencies m = do
        m ^.. field @"declarations" % (genericPlate @(Qualified Text)) % field @"qualifier"

instance HasDependencies (CoreModule (ANF.TopLevelBind Core.Var)) where
    type Key (CoreModule (ANF.TopLevelBind Core.Var)) = ModuleName
    key = view (field @"name")

    dependencies m = do
        m ^.. field @"declarations" % (genericPlate @(Qualified Text)) % field @"qualifier"

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
