{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.Parse.Error where

import Error.Diagnose
import Text.Megaparsec.Error

import Data.Text qualified as T
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Text.Megaparsec qualified as MP
import Prelude hiding (error, lines)

import Control.Lens (folded, mapped, mapping, over, sumOf, to, view)
import Data.Foldable (Foldable (foldl))
import Data.List (lines)
import Data.Set qualified as Set (toList)
import Elara.AST.Region (Located (Located), RealSourceRegion (SourceRegion), SourceRegion (..), sourceRegion, sourceRegionToDiagnosePosition, unlocated)
import Elara.Error (ReportDiagnostic (reportDiagnostic))
import Elara.Lexer.Lexer (Lexeme)
import Elara.Lexer.Token (tokenRepr)

import Elara.Parse.Stream (TokenStream (TokenStream))
import Prelude hiding (error, lines)

data ElaraParseError
    = KeywordUsedAsName Text
    deriving (Eq, Show, Ord)

instance HasHints ElaraParseError Text where
    hints (KeywordUsedAsName kw) =
        [ Note (kw <> " is a keyword which can only be used in certain contexts. However, it was used as a name here.")
        , Hint "Try using a different name"
        ]

class ErrorRegionSize e where
    errorRegion :: e -> Int

instance ErrorRegionSize ElaraParseError where
    errorRegion (KeywordUsedAsName kw) = T.length kw

instance ShowErrorComponent ElaraParseError where
    showErrorComponent (KeywordUsedAsName kw) = "Keyword " <> show kw <> " used as name"

newtype WParseErrorBundle e m = WParseErrorBundle {unWParseErrorBundle :: ParseErrorBundle e m}

instance (HasHints m Text, ShowErrorComponent m) => ReportDiagnostic (WParseErrorBundle TokenStream m) where
    reportDiagnostic (WParseErrorBundle e) = diagnosticFromBundle (const True) (Just "E0001") "Parse error" Nothing e

{- | This is a slightly modified version of 'errorDiagnosticFromBundle' from the 'diagnose' package.
   | It adds the ability to highlight a region of the source code rather than a single point for error highlighting.
-}
diagnosticFromBundle ::
    forall msg s e.
    (MP.Token s ~ Lexeme, IsString msg, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s, Show (MP.Token s)) =>
    -- | How to decide whether this is an error or a warning diagnostic
    (MP.ParseError s e -> Bool) ->
    -- | An optional error code
    Maybe msg ->
    -- | The error message of the diagnostic
    msg ->
    -- | Default hints when trivial errors are reported
    Maybe [Note msg] ->
    -- | The bundle to create a diagnostic from
    MP.ParseErrorBundle s e ->
    Diagnostic msg
diagnosticFromBundle isError code msg (fromMaybe [] -> trivialHints) MP.ParseErrorBundle{..} =
    foldl addReport def (toLabeledPosition <$> bundleErrors)
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

    errorRegion :: MP.ParseError s e -> [SourceRegion]
    errorRegion (MP.TrivialError _ (Just (Tokens ts)) _) = toList $ view sourceRegion <$> ts
    errorRegion (MP.TrivialError{}) = []
    errorRegion _ = []

    errorLength :: MP.ParseError s e -> Int
    errorLength MP.TrivialError{} = 1
    errorLength (MP.FancyError _ errs) = sum (errorLength' <$> Set.toList errs)

    errorLength' :: MP.ErrorFancy e -> Int
    errorLength' (MP.ErrorFail _) = 1
    errorLength' (MP.ErrorIndentation{}) = 1
    errorLength' (MP.ErrorCustom e) = 1

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