{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Elara.Parse.Error.Internal where

import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import Text.Megaparsec.Error

import Data.Foldable (Foldable (foldl))
import Data.List (lines)
import Data.Set qualified as Set (toList)
import Elara.Parse.Error
import Text.Megaparsec qualified as MP
import Prelude hiding (error, lines)

{- | This is a slightly modified version of 'errorDiagnosticFromBundle' from the 'diagnose' package.
   | It adds the ability to highlight a region of the source code rather than a single point for error highlighting.
-}
diagnosticFromBundle ::
    forall msg s e.
    (IsString msg, HasHints e msg, MP.ShowErrorComponent e, MP.VisualStream s, MP.TraversableStream s, ErrorRegionSize e) =>
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
            size = errorLength error
            source = fromSourcePos size (MP.pstateSourcePos pos)
            msgs = fromString <$> lines (MP.parseErrorTextPretty error)
         in flip
                (if isError error then Err code msg else Warn code msg)
                (errorHints error)
                if
                        | [m] <- msgs -> [(source, This m)]
                        | [m1, m2] <- msgs -> [(source, This m1), (source, Where m2)]
                        | otherwise -> [(source, This $ fromString "<<Unknown error>>")]

    errorLength :: MP.ParseError s e -> Int
    errorLength MP.TrivialError{} = 1
    errorLength (MP.FancyError _ errs) = sum (errorLength' <$> Set.toList errs)

    errorLength' :: MP.ErrorFancy e -> Int
    errorLength' (MP.ErrorFail _) = 1
    errorLength' (MP.ErrorIndentation{}) = 1
    errorLength' (MP.ErrorCustom e) = errorRegion e

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