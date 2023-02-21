{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Lens
import Elara.AST.Frontend.Unlocated
import Elara.AST.Module
import Elara.AST.Select
import Elara.Annotate (annotateModule)
import Elara.Annotate.Shunt (fixOperators)
import Elara.Error
import Elara.Parse
import Elara.Parse.Error.Internal
import Error.Diagnose
import Polysemy (run)
import Polysemy.Error (runError)
import Polysemy.Reader
import Polysemy.Writer (runWriter)
import Print (printColored)
import Prelude hiding (runReader)

main :: IO ()
main = do
  s <- loadModule "source.elr"
  p <- loadModule "prelude.elr"
  case liftA2 (,) s p of
    Left err -> printDiagnostic stdout True True 4 defaultStyle err
    Right (source, prelude) ->
      let modules = fromList [(source ^. name, source), (prelude ^. name, prelude)]
       in case run $ runError $ runReader modules (annotateModule source) of
            Left err -> print err
            Right m' -> do
              print m'
              -- let y = run $ runError $ runWriter $ overExpressions (fixOperators (fromList [])) m'
              -- printColored y

loadModule :: FilePath -> IO (Either (Diagnostic Text) (Module Frontend))
loadModule path = do
  s <- decodeUtf8Strict <$> readFileBS path
  case s of
    Left unicodeError -> do
      let errReport = Err Nothing ("Could not read file: " <> fromString path) [] [Note (show unicodeError)]
       in pure . Left $ addReport def errReport
    Right contents ->
      case parse path contents of
        Left parseError ->
          let diag = reportDiagnostic parseError
              diag' = addFile diag path (toString contents)
           in pure (Left diag')
        Right m -> pure (Right m)

-- unlocateModule :: Module Frontend -> Module UnlocatedFrontend
-- unlocateModule = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens %~ stripLocation

-- overExpressions :: Applicative f => (ASTExpr ast -> f (ASTExpr ast)) -> Module ast -> f (Module ast)
-- overExpressions = moduleDeclarations . traverse . _declarationBodyLens . _declarationBodyExpressionLens