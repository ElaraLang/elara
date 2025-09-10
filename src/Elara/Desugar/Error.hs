module Elara.Desugar.Error where

import Elara.AST.Desugared
import Elara.AST.Generic
import Elara.AST.Name
import Elara.AST.Region
import Elara.AST.Select
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Error.Diagnose

data DesugarError
    = DefWithoutLet DesugaredType
    | InfixWithoutDeclaration (Located Name) SourceRegion (ValueDeclAnnotations Desugared)
    | DuplicateDeclaration PartialDeclaration PartialDeclaration
    | DuplicateAnnotations (ValueDeclAnnotations Desugared) (ValueDeclAnnotations Desugared)
    | PartialNamesNotEqual PartialDeclaration PartialDeclaration
    deriving (Typeable, Show)

instance Exception DesugarError

instance ReportableError DesugarError where
    report (DefWithoutLet _) =
        writeReport $ Err (Just Codes.defWithoutLet) "Def without let" [] []
    report (DuplicateDeclaration a b) =
        writeReport $
            Err
                (Just Codes.duplicateDefinition)
                ("Duplicate declaration names:" <+> pretty a)
                [ (sourceRegionToDiagnosePosition $ partialDeclarationSourceRegion b, This "Name is used here")
                , (sourceRegionToDiagnosePosition $ partialDeclarationSourceRegion a, This "And also here")
                ]
                [ Note "Having multiple variables with the same name makes it impossible to tell which one you want to use!"
                , Hint "Rename one of the declarations"
                ]
    report (PartialNamesNotEqual a b) =
        writeReport $ Err (Just Codes.partialNamesNotEqual) ("Partial names not equal: " <+> pretty a <+> "and" <+> pretty b) [] []
    report (InfixWithoutDeclaration n _ l) =
        writeReport $ Err (Just Codes.infixDeclarationWithoutValue) ("Operator fixity declaration without corresponding body: " <+> pretty n <+> "," <+> show l) [] []
    report (DuplicateAnnotations a b) =
        writeReport $ Err (Just Codes.duplicateFixityAnnotations) ("Duplicate fixity annotations" <+> pretty a <+> "and" <+> pretty b) [] []

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
to create a 'Both' declaration, which is then resolved to a 'Desugared.Declaration'
-}
data PartialDeclaration
    = -- | A partial declaration with just a def line
      JustDef
        -- | Name of the declaration
        (Located VarName)
        -- | The *overall* region of the declaration, not just the body!
        SourceRegion
        DesugaredType
        (Maybe (ValueDeclAnnotations Desugared))
    | JustLet
        (Located VarName)
        SourceRegion
        DesugaredExpr
        (Maybe (ValueDeclAnnotations Desugared))
    | JustInfix
        (Located Name)
        SourceRegion
        (ValueDeclAnnotations Desugared)
    | AllDecl (Located VarName) SourceRegion DesugaredType DesugaredExpr (ValueDeclAnnotations Desugared)
    | Immediate Name DesugaredDeclarationBody
    deriving (Typeable, Show)

partialDeclarationSourceRegion :: PartialDeclaration -> SourceRegion
partialDeclarationSourceRegion (JustDef _ sr _ _) = sr
partialDeclarationSourceRegion (JustLet _ sr _ _) = sr
partialDeclarationSourceRegion (JustInfix _ sr _) = sr
partialDeclarationSourceRegion (AllDecl _ sr _ _ _) = sr
partialDeclarationSourceRegion (Immediate _ (DeclarationBody (Located sr _))) = sr

instance Pretty PartialDeclaration where
    pretty (JustDef n _ _ _) = "JustDef" <+> pretty n
    pretty (JustLet n _ _ _) = "JustLet" <+> pretty n
    pretty (JustInfix n _ _) = "JustInfix" <+> pretty n
    pretty (AllDecl n _ _ _ _) = "All" <+> pretty n
    pretty (Immediate n _) = "Immediate" <+> pretty n
