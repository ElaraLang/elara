module Elara.Desugar.Error where

import Elara.AST.Instances ()
import Elara.AST.Location
import Elara.AST.Name
import Elara.AST.Phases.Desugared
import Elara.AST.Phases.Frontend qualified as Frontend
import Elara.AST.Region
import Elara.AST.Types
import Elara.Data.Pretty
import Elara.Desugar.Common
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Error.Diagnose hiding (Annotation)

data DesugarError
    = DefWithoutLet DesugaredType
    | InfixWithoutDeclaration (Located Name) SourceRegion [Annotation SourceRegion Desugared]
    | DuplicateDeclaration PartialDeclaration PartialDeclaration
    | PartialNamesNotEqual PartialDeclaration PartialDeclaration
    | TuplePatternTooShort (Pattern SourceRegion Frontend.Frontend)
    deriving (Typeable, Show, Generic)

instance Exception DesugarError

instance Pretty DesugarError where
    pretty = viaShow

instance ReportableError DesugarError where
    getReport (DefWithoutLet ty) =
        let Type sr _ _ = ty
         in Just $
                Err
                    (Just Codes.defWithoutLet)
                    ("Def without let at" <+> pretty sr)
                    [ (sourceRegionToDiagnosePosition $ unwrapLoc sr, This "Def without let here")
                    ]
                    [ Note "A 'def' must always be followed by a let binding"
                    , Hint "Try adding a 'let' binding after the 'def'"
                    ]
    getReport (DuplicateDeclaration a b) =
        Just $
            Err
                (Just Codes.duplicateDefinition)
                ("Duplicate declaration names:" <+> pretty a)
                [ (sourceRegionToDiagnosePosition $ unwrapLoc $ partialDeclarationSourceRegion b, This "Name is used here")
                , (sourceRegionToDiagnosePosition $ unwrapLoc $ partialDeclarationSourceRegion a, This "And also here")
                ]
                [ Note "Having multiple variables with the same name makes it impossible to tell which one you want to use!"
                , Hint "Rename one of the declarations"
                ]
    getReport (PartialNamesNotEqual a b) =
        Just $ Err (Just Codes.partialNamesNotEqual) ("Partial names not equal: " <+> pretty a <+> "and" <+> pretty b) [] []
    getReport (InfixWithoutDeclaration n _ l) =
        Just $ Err (Just Codes.infixDeclarationWithoutValue) ("Operator fixity declaration without corresponding body: " <+> pretty n <+> "," <+> show l) [] []
    getReport (TuplePatternTooShort p) =
        let Pattern sr _ _ = p
         in Just $
                Err
                    (Just Codes.tuplePatternTooShort)
                    "Tuple patterns must have at least 2 elements"
                    [(sourceRegionToDiagnosePosition $ unwrapLoc sr, This "This tuple pattern is too short")]
                    [ Note "A tuple pattern must have at least 2 elements, e.g. (x, y)"
                    , Note "This is likely an internal error."
                    , Hint "If you want an empty tuple, use ()"
                    ]
