module Elara.Desugar.Error where

import Error.Diagnose hiding (Annotation, Hint, Note)

import Elara.AST.Instances ()
import Elara.AST.Location
import Elara.AST.Name
import Elara.AST.Phases.Desugared (Desugared, DesugaredPattern, DesugaredType)
import Elara.AST.Region
import Elara.Data.Pretty (Pretty (..), viaShow, (<+>))
import Elara.Desugar.Common
import Elara.Error
import Elara.Error.Diagnose (toDiagnoseReports)
import Elara.Lexer.Token (Lexeme)

import Elara.AST.Phases.Frontend qualified as Frontend
import Elara.AST.Types qualified as New
import Elara.Error.Codes qualified as Codes

data DesugarError
    = DefWithoutLet DesugaredType
    | DuplicateDeclaration PartialDeclaration PartialDeclaration
    | PartialNamesNotEqual PartialDeclaration PartialDeclaration
    | InfixWithoutDeclaration (Located Name) (Located (Qualified Name)) Lexeme
    | TuplePatternTooShort (New.Pattern SourceRegion Frontend.Frontend)
    deriving (Generic, Show, Typeable)

instance Exception DesugarError

instance Pretty DesugarError where
    pretty = diagnosticMessage

instance ElaraDiagnostic DesugarError where
    diagnosticMessage (DefWithoutLet _) = "Def without let"
    diagnosticMessage (DuplicateDeclaration a _) = "Duplicate declaration names:" <+> Elara.Data.Pretty.pretty a
    diagnosticMessage (PartialNamesNotEqual a b) = "Partial names not equal: " <+> Elara.Data.Pretty.pretty a <+> "and" <+> Elara.Data.Pretty.pretty b
    diagnosticMessage (InfixWithoutDeclaration n _ l) = "Operator fixity declaration without corresponding body: " <+> Elara.Data.Pretty.pretty n <+> "," <+> show l
    diagnosticMessage (TuplePatternTooShort _) = "Tuple patterns must have at least 2 elements"

    diagnosticCode (DefWithoutLet _) = Just Codes.defWithoutLet
    diagnosticCode (DuplicateDeclaration _ _) = Just Codes.duplicateDefinition
    diagnosticCode (PartialNamesNotEqual _ _) = Just Codes.partialNamesNotEqual
    diagnosticCode (InfixWithoutDeclaration{}) = Just Codes.infixDeclarationWithoutValue
    diagnosticCode (TuplePatternTooShort _) = Just Codes.tuplePatternTooShort

    diagnosticMarkers (DefWithoutLet ty) =
        let New.Type sr _ _ = ty
         in [ElaraMarker (unwrapLoc sr) PrimaryMarker "Def without let here"]
    diagnosticMarkers (DuplicateDeclaration a b) =
        [ ElaraMarker (unwrapLoc $ partialDeclarationSourceRegion b) PrimaryMarker "Name is used here"
        , ElaraMarker (unwrapLoc $ partialDeclarationSourceRegion a) PrimaryMarker "And also here"
        ]
    diagnosticMarkers (PartialNamesNotEqual a _) = [ElaraMarker (unwrapLoc $ partialDeclarationSourceRegion a) PrimaryMarker "Partial names not equal"]
    diagnosticMarkers (InfixWithoutDeclaration n _ _) = [ElaraMarker (n ^. sourceRegion) PrimaryMarker "Operator fixity declaration without corresponding body"]
    diagnosticMarkers (TuplePatternTooShort p) =
        let New.Pattern sr _ _ = p
         in [ElaraMarker (unwrapLoc sr) PrimaryMarker "This tuple pattern is too short"]

    diagnosticNotes (DefWithoutLet _) =
        [ Elara.Error.Note "A 'def' must always be followed by a let binding"
        , Elara.Error.Hint "Try adding a 'let' binding after the 'def'"
        ]
    diagnosticNotes (DuplicateDeclaration _ _) =
        [ Elara.Error.Note "Having multiple variables with the same name makes it impossible to tell which one you want to use!"
        , Elara.Error.Hint "Rename one of the declarations"
        ]
    diagnosticNotes (TuplePatternTooShort _) =
        [ Elara.Error.Note "A tuple pattern must have at least 2 elements, e.g. (x, y)"
        , Elara.Error.Note "This is likely an internal error."
        , Elara.Error.Hint "If you want an empty tuple, use ()"
        ]
    diagnosticNotes _ = []
