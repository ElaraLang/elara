{-# LANGUAGE OverloadedStrings #-}

module Logging where

import Data.Text qualified as T
import Elara.Data.Pretty
import Elara.Logging
import Test.Syd

-- | Local predicate combining level and namespace filtering logic
shouldLog :: LogConfig -> LogLevel -> [T.Text] -> Bool
shouldLog config level ns =
    level >= minLogLevel config
        && case namespaceFilter config of
            Nothing -> True
            Just filterNs -> filterNs `isPrefixOf` ns

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
            toString (prettyToText (pretty Debug)) `shouldContain` "DEBUG"
            toString (prettyToText (pretty Info)) `shouldContain` "INFO"
            toString (prettyToText (pretty Warning)) `shouldContain` "WARN"
            toString (prettyToText (pretty Error)) `shouldContain` "ERROR"

    describe "getLogConfigFromEnv" $ do
        it "returns a valid config" $ do
            config <- getLogConfigFromEnv
            minLogLevel config `shouldSatisfy` (`elem` [Debug, Info, Warning, Error])
