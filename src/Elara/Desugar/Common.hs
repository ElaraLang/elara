module Elara.Desugar.Common where

import Elara.AST.Instances ()
import Elara.AST.Location
import Elara.AST.Name
import Elara.AST.Phases.Desugared
import Elara.AST.Region
import Elara.AST.Types
import Elara.Data.Pretty

{- | A partial declaration stores a desugared part of a declaration
This allows merging of declarations with the same name
For example, the code
@
def a : Int
...
...
let a = 5
@
is legal, and the 2 parts of the declaration need to be merged

Firstly, we create a 'JustDef' after seeing the @def@ line, then we merge this with a 'JustLet' after seeing the @let@ line
to create a 'Both' declaration, which is then resolved to a Desugared.Declaration'
-}
data PartialDeclaration
    = -- | A partial declaration with just a def line
      JustDef
        -- | Name of the declaration
        (TaggedLocate VarNode SourceRegion VarName)
        -- | The *overall* region of the declaration, not just the body!
        (NodeLoc DeclNode SourceRegion)
        DesugaredType
        (Maybe [Annotation SourceRegion Desugared])
    | JustLet
        -- | Name of the declaration
        (TaggedLocate VarNode SourceRegion VarName)
        -- | The *overall* region of the *entire* declaration, not just the body!
        (NodeLoc DeclNode SourceRegion)
        -- | The desugared expression of the let binding
        DesugaredExpr
        -- | Any annotations on the let binding
        (Maybe [Annotation SourceRegion Desugared])
    | -- | A @def@ *and* @let@ declaration that have been merged together, but not yet resolved to a 'Declaration'
      AllDecl
        (TaggedLocate VarNode SourceRegion VarName)
        (NodeLoc DeclNode SourceRegion)
        DesugaredType
        DesugaredExpr
        [Annotation SourceRegion Desugared]
    | Immediate (Declaration SourceRegion Desugared)
    deriving (Typeable, Show, Generic)

partialDeclarationSourceRegion :: PartialDeclaration -> NodeLoc DeclNode SourceRegion
partialDeclarationSourceRegion (JustDef _ sr _ _) = sr
partialDeclarationSourceRegion (JustLet _ sr _ _) = sr
partialDeclarationSourceRegion (AllDecl _ sr _ _ _) = sr
partialDeclarationSourceRegion (Immediate (Declaration sr _)) = sr

instance Pretty PartialDeclaration where
    pretty (JustDef n _ _ _) = "JustDef" <+> pretty n
    pretty (JustLet n _ _ _) = "JustLet" <+> pretty n
    pretty (AllDecl n _ _ _ _) = "All" <+> pretty n
    pretty imm@(Immediate _) = "Immediate" <+> pretty (getPartialName imm)

getPartialName :: PartialDeclaration -> TaggedLocate VarNode SourceRegion Name
getPartialName (JustDef n _ _ _) = fmap toName n
getPartialName (JustLet n _ _ _) = fmap toName n
getPartialName (AllDecl n _ _ _ _) = fmap toName n
getPartialName (Immediate (Declaration _ (Declaration' _ (DeclarationBody _ body)))) =
    case body of
        ValueDeclaration n _ _ _ _ _ -> fmap toName n
        TypeDeclarationBody n _ _ _ _ _ -> NameType <$> retag @VarNode n
        DeclBodyExtension ext -> absurd ext
