# Structured Logging Improvements

This directory contains documentation and examples for the improved structured logging system.

## What's New

The Elara compiler's logging system has been significantly enhanced with the following features:

### 1. **Multiple Log Levels**
- `Debug` - Detailed diagnostic information
- `Info` - General informational messages  
- `Warning` - Warning messages for potential issues
- `Error` - Error messages for serious problems

### 2. **Environment Variable Configuration**
Control logging behavior without code changes:
- `ELARA_DEBUG` - Enable debug logging
- `ELARA_LOG_LEVEL` - Set minimum log level (DEBUG, INFO, WARN, ERROR)
- `ELARA_LOG_TIMESTAMPS` - Show/hide timestamps
- `ELARA_LOG_SOURCE_LOC` - Show/hide source file and line
- `ELARA_LOG_NAMESPACE` - Filter logs by namespace

### 3. **Enhanced Log Output**
- Timestamps (optional)
- Source file and line number (optional)
- Color-coded log levels
- Hierarchical indentation (preserved from original)
- Namespace organization

### 4. **New Logging Functions**
```haskell
-- Simple logging
logDebug, logInfo, logWarning, logError

-- With namespaces
logDebugNS, logInfoNS, logWarningNS, logErrorNS

-- With scoped actions
logDebugWith, logInfoWith
logDebugWithNS, logInfoWithNS
```

### 5. **Backward Compatibility**
All existing `debug*` functions continue to work unchanged.

## Files

- **[logging.md](logging.md)** - Complete documentation of the logging system
- **[examples/LoggingExamples.hs](examples/LoggingExamples.hs)** - Code examples showing various logging patterns

## Quick Start

### Enable Debug Logging

```bash
ELARA_DEBUG=1 elara --run
```

### Show Only Important Messages

```bash
ELARA_LOG_LEVEL=INFO elara --run
```

### Filter by Component

```bash
ELARA_LOG_NAMESPACE=TypeInfer elara --run
```

### Minimal Output (No Timestamps/Locations)

```bash
ELARA_LOG_TIMESTAMPS=false ELARA_LOG_SOURCE_LOC=false elara --run
```

## Example Output

Before:
```
│ generateConstraints: Int 42
│ │ Result: Int
```

After (with all features enabled):
```
2025-12-30 13:07:56 [DEBUG] ConstraintGeneration.hs:65 generateConstraints: Int 42
│ 2025-12-30 13:07:56 [DEBUG] ConstraintGeneration.hs:68 Result: Int
```

After (info level only):
```
[INFO] Type checking completed successfully
```

## Implementation

The implementation is in `src/Elara/Logging.hs` with the following key components:

1. **LogLevel** - Enum for log levels
2. **LogConfig** - Configuration data type
3. **getLogConfigFromEnv** - Read config from environment
4. **structuredDebugToLogWith** - Interpreter with custom config
5. **New logging functions** - Convenience functions for each level

## Migration Guide

Existing code works without changes. To benefit from new features:

1. Replace `debug` with appropriate level:
   ```haskell
   -- Before
   debug "Processing..."
   
   -- After
   logDebug "Processing..."  -- or logInfo, logWarning, logError
   ```

2. Add namespaces for better organization:
   ```haskell
   logDebugNS ["TypeInfer"] "Generating constraints"
   ```

3. Use environment variables to control output without code changes

## See Also

- Main implementation: `src/Elara/Logging.hs`
- Integration: `app/Main.hs`
- Usage examples: Throughout the codebase in type inference, core transformations, etc.
