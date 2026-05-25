{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.Parse.Error where

import Data.Foldable (Foldable (foldl))
import Data.List (lines)
import Error.Diagnose hiding (Hint, Note)
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Text.Megaparsec (unPos)
import Text.Megaparsec.Error

import Data.Set qualified as Set (toList)
import Error.Diagnose qualified as Diag
import Text.Megaparsec qualified as MP

import Elara.AST.Instances ()
import Elara.AST.Location
import Elara.AST.Name (MaybeQualified, ModuleName, VarName)
import Elara.AST.Phase
import Elara.AST.Phases.Frontend (FrontendExpr)
import Elara.AST.Region (Located, SourceRegion, diagnosePositionToSourceRegion, sourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Diagnose (toDiagnoseReports)
import Elara.Lexer.Token (Lexeme)
import Elara.Parse.Stream (TokenStream)
import Prelude hiding (lines)

import Elara.Error.Codes qualified as Codes

data ElaraParseError
    = KeywordUsedAsName (Located (MaybeQualified VarName))
    | EmptyRecord SourceRegion
    | EmptyLambda SourceRegion
    | InfixPrecTooHigh (Located Integer)
    | InvalidConstantExpression {wholeExpr :: FrontendExpr, offendingSection :: FrontendExpr}
    | ModuleNameMismatch
        { expectedName :: ModuleName
        , declaredName :: Located ModuleName
        }
    deriving (Eq, Ord, Show)

parseErrorSources :: ElaraParseError -> [SourceRegion]
parseErrorSources (KeywordUsedAsName l) = [view sourceRegion l]
parseErrorSources (EmptyRecord sr) = [sr]
parseErrorSources (EmptyLambda sr) = [sr]
parseErrorSources (InfixPrecTooHigh l) = [view sourceRegion l]
parseErrorSources (InvalidConstantExpression _a _b) = []
parseErrorSources (ModuleNameMismatch _ declaredLoc) = [view sourceRegion declaredLoc]

instance HasHints ElaraParseError (Doc AnsiStyle) where
    hints (KeywordUsedAsName kw) =
        [ Diag.Note (view (unlocated % to pretty) kw <+> "is a keyword which can only be used in certain contexts. However, it was used as a name here.")
        , Diag.Hint "Try using a different name"
        ]
    hints (EmptyRecord _) =
        [ Diag.Note "Record types cannot be empty."
        , Diag.Hint "Try adding a field to the record type e.g. { x : Int }"
        , Diag.Hint "You may be looking for the unit type, which is written as ()"
        ]
    hints (EmptyLambda _) =
        [Diag.Note "Lambda expressions cannot be empty."]
    hints (InfixPrecTooHigh _) =
        [Diag.Note "The precedence of an infix operator must be between 0 and 9."]
    hints (InvalidConstantExpression _ _) =
        [Diag.Note "This expression cannot be evaluated at compile time."]
    hints (ModuleNameMismatch expected declared) =
        [ Diag.Note ("Module name" <+> squotes (pretty (view unlocated declared)) <+> "does not match the expected name" <+> squotes (pretty expected) <+> "inferred from the file path.")
        , Diag.Hint ("Either rename the file to match" <+> squotes (pretty (view unlocated declared)) <+> "or update the module declaration to" <+> squotes (pretty expected))
        , Diag.Hint "You can also remove the module declaration entirely, and the name will be inferred from the file path."
        ]

instance ShowErrorComponent ElaraParseError where
    showErrorComponent (KeywordUsedAsName kw) = "Keyword " <> show kw <> " used as name"
    showErrorComponent (EmptyRecord _) = "Empty record"
    showErrorComponent (EmptyLambda _) = "Empty lambda"
    showErrorComponent (InfixPrecTooHigh l) = "Infix precedence too high: " <> show l
    showErrorComponent (InvalidConstantExpression _ _) = "Invalid constant expression"
    showErrorComponent (ModuleNameMismatch expected declared) = "Module name mismatch: expected " <> show expected <> " but found " <> show declared

newtype WParseErrorBundle e m = WParseErrorBundle {unWParseErrorBundle :: ParseErrorBundle e m}

deriving instance (Show s, Show (MP.Token s), Show e) => Show (WParseErrorBundle s e)

deriving instance (Eq s, Eq (MP.Token s), Eq e) => Eq (WParseErrorBundle s e)

instance (Show s, Show (MP.Token s), Show e, Typeable s, Typeable (MP.Token s), Typeable e) => Exception (WParseErrorBundle s e)

instance Pretty (WParseErrorBundle TokenStream ElaraParseError) where
    pretty = diagnosticMessage

instance ElaraDiagnostic (WParseErrorBundle TokenStream ElaraParseError) where
    diagnosticMessage (WParseErrorBundle e) = Elara.Data.Pretty.pretty (errorBundlePretty e)
    diagnosticCode _ = Just Codes.genericParseError
    diagnosticReports (WParseErrorBundle e) =
        let codeDoc = reAnnotate (const mempty) (pretty Codes.genericParseError)
            diag = diagnosticFromBundle (const True) (Just codeDoc) (pretty $ errorBundlePretty e) Nothing e
         in fmap diagReportToElaraReport (Diag.reportsOf diag)

diagReportToElaraReport :: Diag.Report (Doc AnsiStyle) -> ElaraReport
diagReportToElaraReport r =
    case r of
        Diag.Err _ msg markers notes ->
            ElaraReport
                { reportSeverity = ErrorSeverity
                , reportCode = Nothing -- Can't easily convert back from Doc to ErrorCode
                , reportMessage = msg
                , reportMarkers = fmap diagMarkerToElaraMarker markers
                , reportNotes = fmap diagNoteToElaraNote notes
                }
        Diag.Warn _ msg markers notes ->
            ElaraReport
                { reportSeverity = WarningSeverity
                , reportCode = Nothing
                , reportMessage = msg
                , reportMarkers = fmap diagMarkerToElaraMarker markers
                , reportNotes = fmap diagNoteToElaraNote notes
                }

diagMarkerToElaraMarker :: (Diag.Position, Diag.Marker (Doc AnsiStyle)) -> ElaraMarker
diagMarkerToElaraMarker (pos, marker) =
    ElaraMarker
        { markerRegion = diagnosePositionToSourceRegion pos
        , markerType = case marker of
            Diag.This{} -> PrimaryMarker
            Diag.Where{} -> SecondaryMarker
            Diag.Maybe{} -> InfoMarker
            Diag.Blank -> InfoMarker
        , markerMessage = case marker of
            Diag.This m -> m
            Diag.Where m -> m
            Diag.Maybe m -> m
            Diag.Blank -> ""
        }

diagNoteToElaraNote :: Diag.Note (Doc AnsiStyle) -> ElaraNote
diagNoteToElaraNote (Diag.Note msg) = Elara.Error.Note msg
diagNoteToElaraNote (Diag.Hint msg) = Elara.Error.Hint msg

{- | This is a slightly modified version of 'errorDiagnosticFromBundle' from the 'diagnose' package.
It adds the ability to highlight a region of the source code rather than a single point for error highlighting.
-}
diagnosticFromBundle ::
    forall msg s e.
    (MP.Token s ~ Lexeme, e ~ ElaraParseError, IsString msg, Pretty msg, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s) =>
    -- | How to decide whether this is an error or a warning diagnostic
    (MP.ParseError s e -> Bool) ->
    -- | An optional error code
    Maybe msg ->
    -- | The error message of the diagnostic
    msg ->
    -- | Default hints when trivial errors are reported
    Maybe [Diag.Note msg] ->
    -- | The bundle to create a diagnostic from
    MP.ParseErrorBundle s ElaraParseError ->
    Diag.Diagnostic msg
diagnosticFromBundle isError code msg (fromMaybe [] -> trivialHints) MP.ParseErrorBundle{..} =
    foldl Diag.addReport mempty (toLabeledPosition <$> bundleErrors)
  where
    toLabeledPosition :: MP.ParseError s e -> Diag.Report msg
    toLabeledPosition error =
        let (_, pos) = MP.reachOffset (MP.errorOffset error) bundlePosState
            source = maybe (fromSourcePos (errorLength error) (MP.pstateSourcePos pos)) sourceRegionToDiagnosePosition (listToMaybe (errorRegion error))
            msgs = fromString <$> lines (MP.parseErrorTextPretty error)
         in flip
                (if isError error then Diag.Err code msg else Diag.Warn code msg)
                (errorHints error)
                if
                    | [m] <- msgs -> [(source, Diag.This m)]
                    | [m1, m2] <- msgs -> [(source, Diag.This m1), (source, Diag.Where m2)]
                    | otherwise -> [(source, Diag.This $ fromString "<<Unknown error>>")]

    errorRegion :: MP.ParseError s ElaraParseError -> [SourceRegion]
    errorRegion (MP.TrivialError _ (Just (MP.Tokens ts)) _) = toList $ view sourceRegion <$> ts
    errorRegion (MP.TrivialError _ (Just (MP.Label _)) _) = []
    errorRegion (MP.TrivialError _ (Just MP.EndOfInput) _) = []
    errorRegion (MP.TrivialError _ Nothing _) = []
    errorRegion (MP.FancyError _ errs) =
        Set.toList errs >>= \case
            MP.ErrorFail _ -> []
            MP.ErrorIndentation{} -> []
            MP.ErrorCustom e -> parseErrorSources e

    errorLength :: MP.ParseError s e -> Int
    errorLength MP.TrivialError{} = 1
    errorLength (MP.FancyError _ errs) = sum (errorLength' <$> Set.toList errs)

    errorLength' :: MP.ErrorFancy e -> Int
    errorLength' (MP.ErrorFail _) = 1
    errorLength' (MP.ErrorIndentation _ a b) = unPos b - unPos a
    errorLength' (MP.ErrorCustom _) = 1

    fromSourcePos :: Int -> MP.SourcePos -> Position
    fromSourcePos size MP.SourcePos{..} =
        let start = both MP.unPos (sourceLine, sourceColumn)
            end = second (+ size) start
         in Position start end sourceName

    errorHints :: MP.ParseError s e -> [Diag.Note msg]
    errorHints MP.TrivialError{} = trivialHints
    errorHints (MP.FancyError _ errs) =
        Set.toList errs >>= \case
            MP.ErrorCustom e -> hints e
            _ -> mempty

both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)
