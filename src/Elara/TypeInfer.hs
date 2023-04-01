module Elara.TypeInfer where

import Control.Lens
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Elara.AST.Shunted qualified as Shunted
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.AST.Typed
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.TypeInfer.AssignIds (assignIds)
import Elara.TypeInfer.Environment
import Elara.TypeInfer.GenerateEquations (TypeEquation, generateEquations)
import Elara.TypeInfer.SubstitutionMap
import Elara.TypeInfer.Unify (unifyAllEquations)
import Polysemy hiding (transform)
import Polysemy.Error (runError)
import Polysemy.State
import Polysemy.Writer
import Print (debugColored, printColored)
import Prelude hiding (Type, runState)

infer :: Shunted.Expr -> IO ()
infer e = runM $ do
    (eqs, (env, (e', _))) <- uniqueGenToIO $ runWriter $ runState (TypeEnvironment mempty) $ do
        env <- assignIds e
        generateEquations (fst env)
        pure env

    let y = run $ runError $ unifyAllEquations (toList eqs) env mempty
    case y of
        Left e -> embed $ printColored e
        Right subst -> do
            e'' <- uniqueGenToIO $ runError $ substitute subst e'
            let stripped = pretty . stripLocation <$> e''
            case stripped of
                Left e -> embed $ printColored e
                Right e -> embed $ putDoc e
