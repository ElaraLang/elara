module Elara.Desugar.Error where

import Elara.AST.Name
import Elara.AST.New.Instances ()
import Elara.AST.New.Phases.Desugared
import Elara.AST.New.Phases.Frontend qualified as Frontend
import Elara.AST.New.Types qualified as New
import Elara.AST.Region
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Error.Diagnose

data DesugarError
    = DefWithoutLet DesugaredType
    | InfixWithoutDeclaration (Located Name) SourceRegion [New.Annotation SourceRegion Desugared]
    | DuplicateDeclaration PartialDeclaration PartialDeclaration
    | PartialNamesNotEqual PartialDeclaration PartialDeclaration
    | TuplePatternTooShort (New.Pattern SourceRegion Frontend.Frontend)
    deriving (Typeable, Show, Generic)

instance Exception DesugarError

instance Pretty DesugarError where
    pretty = viaShow

instance ReportableError DesugarError where
    getReport (DefWithoutLet ty) =
        let New.Type sr _ _ = ty
         in Just $
                Err
                    (Just Codes.defWithoutLet)
                    ("Def without let at" <+> pretty sr)
                    [ (sourceRegionToDiagnosePosition sr, This "Def without let here")
                    ]
                    [ Note "A 'def' must always be followed by a let binding"
                    , Hint "Try adding a 'let' binding after the 'def'"
                    ]
    getReport (DuplicateDeclaration a b) =
        Just $
            Err
                (Just Codes.duplicateDefinition)
                ("Duplicate declaration names:" <+> pretty a)
                [ (sourceRegionToDiagnosePosition $ partialDeclarationSourceRegion b, This "Name is used here")
                , (sourceRegionToDiagnosePosition $ partialDeclarationSourceRegion a, This "And also here")
                ]
                [ Note "Having multiple variables with the same name makes it impossible to tell which one you want to use!"
                , Hint "Rename one of the declarations"
                ]
    getReport (PartialNamesNotEqual a b) =
        Just $ Err (Just Codes.partialNamesNotEqual) ("Partial names not equal: " <+> pretty a <+> "and" <+> pretty b) [] []
    getReport (InfixWithoutDeclaration n _ l) =
        Just $ Err (Just Codes.infixDeclarationWithoutValue) ("Operator fixity declaration without corresponding body: " <+> pretty n <+> "," <+> show l) [] []
    getReport (TuplePatternTooShort p) =
        let New.Pattern sr _ _ = p
         in Just $
                Err
                    (Just Codes.tuplePatternTooShort)
                    "Tuple patterns must have at least 2 elements"
                    [(sourceRegionToDiagnosePosition sr, This "This tuple pattern is too short")]
                    [ Note "A tuple pattern must have at least 2 elements, e.g. (x, y)"
                    , Note "This is likely an internal error, as these cases should be caught by the parser"
                    , Hint "If you want an empty tuple, use ()"
                    ]

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
        (Located VarName)
        -- | The *overall* region of the declaration, not just the body!
        SourceRegion
        DesugaredType
        (Maybe [New.Annotation SourceRegion Desugared])
    | JustLet
        (Located VarName)
        SourceRegion
        DesugaredExpr
        (Maybe [New.Annotation SourceRegion Desugared])
    | AllDecl (Located VarName) SourceRegion DesugaredType DesugaredExpr [New.Annotation SourceRegion Desugared]
    | Immediate Name (New.DeclarationBody SourceRegion Desugared)
    deriving (Typeable, Show, Generic)

partialDeclarationSourceRegion :: PartialDeclaration -> SourceRegion
partialDeclarationSourceRegion (JustDef _ sr _ _) = sr
partialDeclarationSourceRegion (JustLet _ sr _ _) = sr
partialDeclarationSourceRegion (AllDecl _ sr _ _ _) = sr
partialDeclarationSourceRegion (Immediate _ (New.DeclarationBody sr _)) = sr

instance Pretty PartialDeclaration where
    pretty (JustDef n _ _ _) = "JustDef" <+> pretty n
    pretty (JustLet n _ _ _) = "JustLet" <+> pretty n
    pretty (AllDecl n _ _ _ _) = "All" <+> pretty n
    pretty (Immediate n _) = "Immediate" <+> pretty n
