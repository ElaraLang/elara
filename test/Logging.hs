{-# LANGUAGE OverloadedStrings #-}

module LoggingSpec where

import Data.Text qualified as T
import Effectful
import Effectful.Colog qualified as Log
import Elara.Data.Pretty
import Elara.Logging
import Test.Hspec

-- | Test that log levels are correctly ordered
spec :: Spec
spec = do
    describe "LogLevel" $ do
        it "orders log levels correctly" $ do
            Debug `shouldSatisfy` (< Info)
            Info `shouldSatisfy` (< Warning)
            Warning `shouldSatisfy` (< Error)

    describe "LogConfig" $ do
        it "has sensible defaults" $ do
            let config = defaultLogConfig
            minLogLevel config `shouldBe` Debug
            showTimestamps config `shouldBe` False
            showSourceLoc config `shouldBe` False
            namespaceFilter config `shouldBe` Nothing

    describe "shouldLog" $ do
        it "filters by log level" $ do
            let config = defaultLogConfig{minLogLevel = Warning}
            shouldLog config Debug [] `shouldBe` False
            shouldLog config Info [] `shouldBe` False
            shouldLog config Warning [] `shouldBe` True
            shouldLog config Error [] `shouldBe` True

        it "filters by namespace" $ do
            let config = defaultLogConfig{namespaceFilter = Just ["TypeInfer"]}
            shouldLog config Debug ["TypeInfer"] `shouldBe` True
            shouldLog config Debug ["TypeInfer", "Unification"] `shouldBe` True
            shouldLog config Debug ["Parser"] `shouldBe` False
            shouldLog config Debug [] `shouldBe` False

    describe "Pretty LogLevel" $ do
        it "formats log levels" $ do
            prettyToText (pretty Debug) `shouldContain` "DEBUG"
            prettyToText (pretty Info) `shouldContain` "INFO"
            prettyToText (pretty Warning) `shouldContain` "WARN"
            prettyToText (pretty Error) `shouldContain` "ERROR"

    describe "getLogConfigFromEnv" $ do
        it "reads environment variables" $ do
            -- This test would need environment setup
            -- For now, just verify the function exists
            _ <- getLogConfigFromEnv
            pure ()

-- Helper to run tests
main :: IO ()
main = hspec spec
