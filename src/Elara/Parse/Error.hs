{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Elara.Parse.Error where

import Data.List (lines)
import Error.Diagnose hiding (Hint, Note)
import Text.Megaparsec (unPos)
import Text.Megaparsec.Error

import Data.Set qualified as Set (toList)
import Text.Megaparsec qualified as MP

import Elara.AST.Instances ()
import Elara.AST.Name (MaybeQualified, ModuleName, VarName)
import Elara.AST.Phases.Frontend (FrontendExpr)
import Elara.AST.Region (Located, SourceRegion, diagnosePositionToSourceRegion, sourceRegion, unlocated)
import Elara.Data.Pretty
import Elara.Error
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

hints :: ElaraParseError -> [ElaraNote]
hints (KeywordUsedAsName kw) =
    [ Note (view (unlocated % to pretty) kw <+> "is a keyword which can only be used in certain contexts. However, it was used as a name here.")
    , Hint "Try using a different name"
    ]
hints (EmptyRecord _) =
    [ Note "Record types cannot be empty."
    , Hint "Try adding a field to the record type e.g. { x : Int }"
    , Hint "You may be looking for the unit type, which is written as ()"
    ]
hints (EmptyLambda _) =
    [Note "Lambda expressions cannot be empty."]
hints (InfixPrecTooHigh _) =
    [Note "The precedence of an infix operator must be between 0 and 9."]
hints (InvalidConstantExpression _ _) =
    [Note "This expression cannot be evaluated at compile time."]
hints (ModuleNameMismatch expected declared) =
    [ Note ("Module name" <+> squotes (pretty (view unlocated declared)) <+> "does not match the expected name" <+> squotes (pretty expected) <+> "inferred from the file path.")
    , Hint ("Either rename the file to match" <+> squotes (pretty (view unlocated declared)) <+> "or update the module declaration to" <+> squotes (pretty expected))
    , Hint "You can also remove the module declaration entirely, and the name will be inferred from the file path."
    ]
hints (ExpectedIndentation introducer _) =
    [ Note ("The previous line ends after" <+> pretty (quoted introducer) <> ", but this line isn't indented further than the enclosing block, so it starts a new item instead of continuing that body.")
    , Hint ("Indent the body after" <+> pretty (quoted introducer) <+> "on the next line, or write it on the same line.")
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
    diagnosticReports (WParseErrorBundle e) = parseErrorBundleReports e

diagNoteToElaraNote :: Diag.Note (Doc AnsiStyle) -> ElaraNote
diagNoteToElaraNote (Diag.Note msg) = Elara.Error.Note msg
diagNoteToElaraNote (Diag.Hint msg) = Elara.Error.Hint msg

{- | Builds one 'ElaraReport' per error in the bundle.

Loosely on 'errorDiagnosticFromBundle' from the @diagnose@ package, but can highlight a whole region of
source code rather than a single point, and gives each report the 'ErrorCode' of its 'parseErrorCode'.
-}
parseErrorBundleReports :: MP.ParseErrorBundle TokenStream ElaraParseError -> [ElaraReport]
parseErrorBundleReports bundle@MP.ParseErrorBundle{..} = toReport <$> toList bundleErrors
  where
    toReport :: MP.ParseError TokenStream ElaraParseError -> ElaraReport
    toReport err =
        let (_, pos) = MP.reachOffset (MP.errorOffset err) bundlePosState
            region = fromMaybe (diagnosePositionToSourceRegion (fromSourcePos (errorLength err) (MP.pstateSourcePos pos))) (listToMaybe (errorRegion err))
            marker markerType msg = ElaraMarker{markerRegion = region, markerType, markerMessage = fromString msg}
         in ElaraReport
                { reportSeverity = ErrorSeverity
                , reportCode = Just Codes.genericParseError
                , reportMessage = pretty (errorBundlePretty bundle)
                , reportMarkers = case lines (MP.parseErrorTextPretty err) of
                    [m] -> [marker PrimaryMarker m]
                    [m1, m2] -> [marker PrimaryMarker m1, marker SecondaryMarker m2]
                    _ -> [marker PrimaryMarker "<<Unknown error>>"]
                , reportNotes = errorNotes err
                }

    errorRegion :: MP.ParseError TokenStream ElaraParseError -> [SourceRegion]
    errorRegion (MP.TrivialError _ (Just (MP.Tokens ts)) _) = toList $ view sourceRegion <$> ts
    errorRegion (MP.TrivialError _ (Just (MP.Label _)) _) = []
    errorRegion (MP.TrivialError _ (Just MP.EndOfInput) _) = []
    errorRegion (MP.TrivialError _ Nothing _) = []
    errorRegion (MP.FancyError _ errs) =
        Set.toList errs >>= \case
            MP.ErrorFail _ -> []
            MP.ErrorIndentation{} -> []
            MP.ErrorCustom e -> parseErrorSources e

    errorLength :: MP.ParseError TokenStream ElaraParseError -> Int
    errorLength MP.TrivialError{} = 1
    errorLength (MP.FancyError _ errs) = sum (errorLength' <$> Set.toList errs)

    errorLength' :: MP.ErrorFancy ElaraParseError -> Int
    errorLength' (MP.ErrorFail _) = 1
    errorLength' (MP.ErrorIndentation _ a b) = unPos b - unPos a
    errorLength' (MP.ErrorCustom _) = 1

    fromSourcePos :: Int -> MP.SourcePos -> Position
    fromSourcePos size MP.SourcePos{..} =
        let start = both MP.unPos (sourceLine, sourceColumn)
            end = second (+ size) start
         in Position start end sourceName

    errorNotes :: MP.ParseError TokenStream ElaraParseError -> [ElaraNote]
    errorNotes MP.TrivialError{} = []
    errorNotes (MP.FancyError _ errs) =
        Set.toList errs >>= \case
            MP.ErrorCustom e -> hints e
            _ -> mempty

both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)
