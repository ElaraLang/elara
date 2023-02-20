{-# LANGUAGE ViewPatterns #-}

module Elara.Error where

import Error.Diagnose

class ReportableError e where
    reportDiagnostic :: e -> Diagnostic Text
    report :: e -> Report Text

    reportDiagnostic = addReport def . report
    {-# MINIMAL report #-}

collectErrors :: [Either (Diagnostic Text) a] -> Either (Diagnostic Text) [a]
collectErrors (partitionEithers -> partitioned) =
    case partitioned of
        ([], xs) -> Right xs
        (es, _) -> Left (foldr (<>) def es)
