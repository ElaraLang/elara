module Elara.TypeInfer where

import Elara.AST.Shunted qualified as Shunted
import Elara.AST.StripLocation (StripLocation (stripLocation))
import Elara.Data.Pretty
import Elara.Data.Unique
import Elara.TypeInfer.AssignIds (assignIds)
import Elara.TypeInfer.Environment
import Elara.TypeInfer.Finalise (finaliseExpr, runFinalise)
import Elara.TypeInfer.GenerateEquations (generateEquations)
import Elara.TypeInfer.SubstitutionMap
import Elara.TypeInfer.Unify (unifyAllEquations)
import Polysemy hiding (transform)
import Polysemy.Error (runError)
import Polysemy.State
import Polysemy.Writer
import Prettyprinter.Render.Text
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
            -- embed (printColored eqs)
            -- embed (printColored env)
            e'' <- uniqueGenToIO $ runError $ runFinalise $ (substitute subst >=> finaliseExpr) e'
            let stripped = pretty . stripLocation <$> e''
            case stripped of
                Left e -> embed $ printColored e
                Right e -> embed $ putDoc e
