module Elara.Desugar.Error where

import Elara.AST.Instances ()
import Elara.AST.Location
import Elara.AST.Name
import Elara.AST.Phases.Desugared (DesugaredType)
import Elara.AST.Region
import Elara.Data.Pretty (Pretty (..), (<+>))
import Elara.Desugar.Common
import Elara.Error
import Elara.Lexer.Token (Lexeme)

import Elara.AST.Phases.Frontend qualified as Frontend
import Elara.AST.Types qualified as New
import Elara.Data.Pretty.Styles qualified as Styles
import Elara.Error.Codes qualified as Codes

data LetKind = PlainLet | LetBang
    deriving (Eq, Generic, Show)

letKeyword :: IsString s => LetKind -> s
letKeyword PlainLet = "let"
letKeyword LetBang = "let!"

data DesugarError
    = DefWithoutLet DesugaredType
    | DuplicateDeclaration PartialDeclaration PartialDeclaration
    | PartialNamesNotEqual PartialDeclaration PartialDeclaration
    | InfixWithoutDeclaration (Located Name) (Located (Qualified Name)) Lexeme
    | TuplePatternTooShort (New.Pattern SourceRegion Frontend.Frontend)
    | -- | A block ends with a statement (a @let@ or @let!@) rather than an expression.
      BlockEndsWithLet
        -- | which kind of let ended the block
        LetKind
        -- | the offending statement's location
        (NodeLoc ExprNode SourceRegion)
        -- | the entire block's location
        (NodeLoc ExprNode SourceRegion)
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
    diagnosticMessage (BlockEndsWithLet kind _ _) = "Block ends with a" <+> letKeyword kind <+> "binding"

    diagnosticCode (DefWithoutLet _) = Just Codes.defWithoutLet
    diagnosticCode (DuplicateDeclaration _ _) = Just Codes.duplicateDefinition
    diagnosticCode (PartialNamesNotEqual _ _) = Just Codes.partialNamesNotEqual
    diagnosticCode (InfixWithoutDeclaration{}) = Just Codes.infixDeclarationWithoutValue
    diagnosticCode (TuplePatternTooShort _) = Just Codes.tuplePatternTooShort
    diagnosticCode (BlockEndsWithLet{}) = Just Codes.blockEndsWithLet

    diagnosticMarkers (DefWithoutLet ty) =
        let New.Type sr _ _ = ty
         in [ElaraMarker (view sourceRegion sr) PrimaryMarker "Def without let here"]
    diagnosticMarkers (DuplicateDeclaration a b) =
        [ ElaraMarker (view sourceRegion $ partialDeclarationSourceRegion b) PrimaryMarker "Name is used here"
        , ElaraMarker (view sourceRegion $ partialDeclarationSourceRegion a) PrimaryMarker "And also here"
        ]
    diagnosticMarkers (PartialNamesNotEqual a _) = [ElaraMarker (view sourceRegion $ partialDeclarationSourceRegion a) PrimaryMarker "Partial names not equal"]
    diagnosticMarkers (InfixWithoutDeclaration n _ _) = [ElaraMarker (n ^. sourceRegion) PrimaryMarker "Operator fixity declaration without corresponding body"]
    diagnosticMarkers (TuplePatternTooShort p) =
        let New.Pattern sr _ _ = p
         in [ElaraMarker (sr ^. sourceRegion) PrimaryMarker "This tuple pattern is too short"]
    diagnosticMarkers (BlockEndsWithLet kind letLoc blockLoc) =
        ElaraMarker (view sourceRegion letLoc) PrimaryMarker ("this " <> letKeyword kind <> " has no body")
            : [ElaraMarker (view sourceRegion blockLoc) SecondaryMarker "inside this block" | blockLoc /= letLoc]

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
    diagnosticNotes (BlockEndsWithLet kind _ _) =
        [ Elara.Error.Note ("Blocks must end with an expression, not a " <> Styles.keyword (letKeyword kind) <> " binding.")
        , Elara.Error.Hint ("Perhaps you meant to use a " <> Styles.keyword (letKeyword kind <> " ... in") <+> "construct?")
        ]
    diagnosticNotes _ = []
