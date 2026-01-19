{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.Parse.Error where

import Data.Foldable (Foldable (foldl))
import Data.List (lines)
import Data.Set qualified as Set (toList)
import Elara.AST.Frontend (FrontendExpr)
import Elara.AST.Name (MaybeQualified, ModuleName, VarName)
import Elara.AST.Region (Located, SourceRegion, sourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.Data.Pretty
import Elara.Error
import Elara.Error.Codes qualified as Codes
import Elara.Lexer.Token (Lexeme)
import Elara.Parse.Stream (TokenStream)
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import TODO (todo)
import Text.Megaparsec (unPos)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error
import Prelude hiding (lines)

data ElaraParseError
    = KeywordUsedAsName (Located (MaybeQualified VarName))
    | EmptyRecord SourceRegion
    | EmptyLambda SourceRegion
    | InfixPrecTooHigh (Located Integer)
    | InvalidConstantExpression {wholeExpr :: FrontendExpr, offendingSection :: FrontendExpr}
    | ModuleNameMismatch {expectedName :: ModuleName, declaredName :: Located ModuleName}
    deriving (Eq, Show, Ord)

parseErrorSources :: ElaraParseError -> [SourceRegion]
parseErrorSources (KeywordUsedAsName l) = [view sourceRegion l]
parseErrorSources (EmptyRecord sr) = [sr]
parseErrorSources (EmptyLambda sr) = [sr]
parseErrorSources (InfixPrecTooHigh l) = [view sourceRegion l]
parseErrorSources (InvalidConstantExpression a b) = [todo]
parseErrorSources (ModuleNameMismatch _ declaredLoc) = [view sourceRegion declaredLoc]

instance HasHints ElaraParseError (Doc AnsiStyle) where
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

instance ReportableError (WParseErrorBundle TokenStream ElaraParseError) where
    report (WParseErrorBundle e) =
        writeDiagnostic $
            diagnosticFromBundle (const True) (Just Codes.genericParseError) (pretty $ errorBundlePretty e) Nothing e

{- | This is a slightly modified version of 'errorDiagnosticFromBundle' from the 'diagnose' package.
It adds the ability to highlight a region of the source code rather than a single point for error highlighting.
-}
diagnosticFromBundle ::
    forall msg s e.
    (MP.Token s ~ Lexeme, e ~ ElaraParseError, IsString msg, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s) =>
    -- | How to decide whether this is an error or a warning diagnostic
    (MP.ParseError s e -> Bool) ->
    -- | An optional error code
    Maybe msg ->
    -- | The error message of the diagnostic
    msg ->
    -- | Default hints when trivial errors are reported
    Maybe [Note msg] ->
    -- | The bundle to create a diagnostic from
    MP.ParseErrorBundle s ElaraParseError ->
    Diagnostic msg
diagnosticFromBundle isError code msg (fromMaybe [] -> trivialHints) MP.ParseErrorBundle{..} =
    foldl addReport mempty (toLabeledPosition <$> bundleErrors)
  where
    toLabeledPosition :: MP.ParseError s e -> Report msg
    toLabeledPosition error =
        let (_, pos) = MP.reachOffset (MP.errorOffset error) bundlePosState
            source = maybe (fromSourcePos (errorLength error) (MP.pstateSourcePos pos)) sourceRegionToDiagnosePosition (listToMaybe (errorRegion error))
            msgs = fromString <$> lines (MP.parseErrorTextPretty error)
         in flip
                (if isError error then Err code msg else Warn code msg)
                (errorHints error)
                if
                    | [m] <- msgs -> [(source, This m)]
                    | [m1, m2] <- msgs -> [(source, This m1), (source, Where m2)]
                    | otherwise -> [(source, This $ fromString "<<Unknown error>>")]

    errorRegion :: MP.ParseError s ElaraParseError -> [SourceRegion]
    errorRegion (MP.TrivialError _ (Just (Tokens ts)) _) = toList $ view sourceRegion <$> ts
    errorRegion (MP.TrivialError{}) = error "Impossible: TrivialError without tokens"
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

    errorHints :: MP.ParseError s e -> [Note msg]
    errorHints MP.TrivialError{} = trivialHints
    errorHints (MP.FancyError _ errs) =
        Set.toList errs >>= \case
            MP.ErrorCustom e -> hints e
            _ -> mempty

both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)
