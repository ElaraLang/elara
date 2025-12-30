{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Example.Logging
Description : Examples of using the structured logging system
Copyright   : (c) Elara Lang, 2025
License     : MIT

This module demonstrates various ways to use the structured logging system.
-}
module Example.Logging where

import Effectful
import Elara.Data.Pretty
import Elara.Logging

-- | Example 1: Basic logging at different levels
basicLoggingExample :: StructuredDebug :> r => Eff r ()
basicLoggingExample = do
    logDebug "Starting application"
    logInfo "Processing user request"
    logWarning "Deprecated function called"
    logError "Failed to connect to database"

-- | Example 2: Using namespaces to organize logs
namespacedLoggingExample :: StructuredDebug :> r => Eff r ()
namespacedLoggingExample = do
    logInfoNS ["Server"] "Server started on port 8080"
    logDebugNS ["Server", "Router"] "Registered 5 routes"
    logDebugNS ["Database"] "Connection pool initialized"
    logDebugNS ["Database", "Migration"] "Running migrations"

-- | Example 3: Scoped logging with automatic indentation
scopedLoggingExample :: StructuredDebug :> r => Eff r Int
scopedLoggingExample = do
    logInfoWith "Computing factorial of 5" $ do
        logDebug "Starting computation"
        let result = product [1 .. 5]
        logDebug $ "Result: " <> pretty result
        pure result

-- | Example 4: Combining namespaces and scopes
complexLoggingExample :: StructuredDebug :> r => Eff r ()
complexLoggingExample = do
    logInfoWithNS ["TypeInfer"] "Type checking module" $ do
        logDebug "Generating constraints"
        logDebugWith "Processing declarations" $ do
            logDebug "Declaration 1: function definition"
            logDebug "Declaration 2: type alias"
        logInfo "Constraints generated successfully"

-- | Example 5: Legacy debug functions (backward compatibility)
legacyLoggingExample :: StructuredDebug :> r => Eff r String
legacyLoggingExample = do
    debug "Using legacy debug function"
    debugWith "Processing with old API" $ do
        debug "Nested log message"
        pure "result"

-- | Example 6: Real-world type inference scenario
typeInferenceExample :: StructuredDebug :> r => Eff r ()
typeInferenceExample = do
    logInfoWithNS ["TypeInfer"] "Inferring types for module Main" $ do
        logDebugWithNS ["TypeInfer"] "Processing function: factorial" $ do
            logDebug "Parameter: n : Int"
            logDebugWith "Inferring body type" $ do
                logDebug "Expression: if n == 0 then 1 else n * factorial (n - 1)"
                logDebug "Condition type: Bool"
                logDebug "Then branch type: Int"
                logDebug "Else branch type: Int"
            logInfo "Inferred type: Int -> Int"

        logDebugWithNS ["TypeInfer"] "Processing function: main" $ do
            logDebug "No parameters"
            logDebugWith "Inferring body type" $ do
                logDebug "Expression: print (factorial 5)"
                logDebug "Argument type: Int"
            logInfo "Inferred type: IO ()"

        logInfo "Type inference completed successfully"

{- |
Example output when running with different configurations:

With full logging (ELARA_LOG_LEVEL=DEBUG):
  2025-12-30 13:07:56 [INFO] Example.hs:52 [TypeInfer] Inferring types for module Main
  │ 2025-12-30 13:07:56 [DEBUG] Example.hs:53 [TypeInfer] Processing function: factorial
  │ │ 2025-12-30 13:07:56 [DEBUG] Example.hs:54 Parameter: n : Int
  │ │ 2025-12-30 13:07:56 [DEBUG] Example.hs:55 Inferring body type
  │ │ │ 2025-12-30 13:07:56 [DEBUG] Example.hs:56 Expression: if n == 0 then 1 else n * factorial (n - 1)
  │ │ │ 2025-12-30 13:07:56 [DEBUG] Example.hs:57 Condition type: Bool
  │ │ │ 2025-12-30 13:07:56 [DEBUG] Example.hs:58 Then branch type: Int
  │ │ │ 2025-12-30 13:07:56 [DEBUG] Example.hs:59 Else branch type: Int
  │ │ 2025-12-30 13:07:56 [INFO] Example.hs:60 Inferred type: Int -> Int

With info logging (ELARA_LOG_LEVEL=INFO):
  2025-12-30 13:07:56 [INFO] Example.hs:52 [TypeInfer] Inferring types for module Main
  │ 2025-12-30 13:07:56 [INFO] Example.hs:60 Inferred type: Int -> Int
  │ 2025-12-30 13:07:56 [INFO] Example.hs:66 Inferred type: IO ()
  2025-12-30 13:07:56 [INFO] Example.hs:68 Type inference completed successfully

With namespace filtering (ELARA_LOG_NAMESPACE=TypeInfer):
  Only messages with [TypeInfer] namespace will be shown
-}
