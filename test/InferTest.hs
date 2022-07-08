module InferTest where

import Control.Monad (replicateM)
import Data.List (nub)
import Test.Hspec
import TypeInfer.Env (emptyEnv, freshTVar, withCopyOfEnv)
import TypeInfer.Infer (runInfer)

spec :: Spec
spec = describe "Test Inference" $ do
  it "Should always produce unique TVars" $ do
    let count = 1000
    let (Right (tvs, _, _)) = runInfer emptyEnv $ do
          let someTVars = replicateM count freshTVar
          tv1 <- someTVars
          tv2 <- withCopyOfEnv someTVars
          tv3 <- withCopyOfEnv someTVars
          tv4 <- withCopyOfEnv $ withCopyOfEnv someTVars
          return $ concat [tv1, tv2, tv3, tv4]
    let all = nub tvs
    length all `shouldBe` (count * 4)