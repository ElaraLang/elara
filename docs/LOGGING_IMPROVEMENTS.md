# Structured Logging System - Improvement Summary

## Overview

The Elara compiler's structured debug and logging system has been significantly enhanced to provide better visibility into compiler operations, easier debugging, and more control over log output.

## Key Improvements

### 1. Multiple Log Levels ✨

Previously, all logs were at a single "debug" level. Now there are four distinct levels:

- **Debug**: Detailed diagnostic information for deep debugging
- **Info**: General informational messages about major milestones
- **Warning**: Warnings about potential issues or deprecated usage
- **Error**: Error messages for serious problems

Each level is color-coded for easy visual scanning.

### 2. Environment-Based Configuration 🔧

Control logging without changing code:

| Environment Variable | Purpose | Example |
|---------------------|---------|---------|
| `ELARA_DEBUG` | Enable debug logging | `ELARA_DEBUG=1` |
| `ELARA_LOG_LEVEL` | Set minimum level | `ELARA_LOG_LEVEL=INFO` |
| `ELARA_LOG_TIMESTAMPS` | Toggle timestamps | `ELARA_LOG_TIMESTAMPS=false` |
| `ELARA_LOG_SOURCE_LOC` | Toggle source locations | `ELARA_LOG_SOURCE_LOC=false` |
| `ELARA_LOG_NAMESPACE` | Filter by namespace | `ELARA_LOG_NAMESPACE=TypeInfer` |

### 3. Rich Log Metadata 📊

Every log message can now include:

- **Timestamp**: When the message was logged (optional, disabled by default)
- **Log Level**: Visual indicator of importance (always shown)
- **Source Location**: File name and line number where the log was called (optional, disabled by default)
- **Namespace**: Component or module that generated the log (optional)
- **Hierarchical Indentation**: Shows call depth and structure (preserved from original)

**Default (clean output for simple debugging):**
```
[DEBUG] [TypeInfer.Unification] Unifying types
```

**With all metadata enabled:**
```
2025-12-30 13:07:56 [DEBUG] TypeInfer.hs:45 [TypeInfer.Unification] Unifying types
```

**Note**: Timestamps and source locations are disabled by default to keep output readable. Enable them with environment variables when you need detailed diagnostics.

### 4. Namespace Filtering 🎯

Large applications can produce overwhelming amounts of logs. The namespace filtering feature allows focusing on specific components:

```bash
# See only type inference logs
ELARA_LOG_NAMESPACE=TypeInfer elara --run

# See only parser logs
ELARA_LOG_NAMESPACE=Parser elara --run
```

Namespaces are hierarchical - filtering for `TypeInfer` will also show `TypeInfer.Unification`, `TypeInfer.Generalise`, etc.

### 5. New API Functions 🛠️

Convenient functions for each log level:

```haskell
-- Simple logging
logDebug "Detailed trace information"
logInfo "Module compiled successfully"
logWarning "Deprecated function used"
logError "Type mismatch detected"

-- With namespaces
logDebugNS ["TypeInfer", "Unification"] "Starting unification"

-- With scoped blocks (automatic indentation)
result <- logInfoWith "Type checking module" $ do
    -- logs here are indented
    checkTypes module
```

### 6. Backward Compatibility ✅

All existing `debug`, `debugWith`, `debugNS`, and `debugWithNS` functions continue to work exactly as before, treated as Debug-level logs.

## Usage Examples

### Development: Clean Debug Output (Default)

```bash
ELARA_DEBUG=1 elara --run
# Shows: [DEBUG] Message here
```

### Production: Important Messages Only

```bash
ELARA_LOG_LEVEL=INFO elara --run
```

### Debugging with Timestamps

```bash
ELARA_DEBUG=1 ELARA_LOG_TIMESTAMPS=true elara --run
# Shows: 2025-12-30 13:07:56 [DEBUG] Message here
```

### Debugging with Full Diagnostics

```bash
ELARA_DEBUG=1 ELARA_LOG_TIMESTAMPS=true ELARA_LOG_SOURCE_LOC=true elara --run
# Shows: 2025-12-30 13:07:56 [DEBUG] File.hs:123 Message here
```

### Debugging Specific Component

```bash
ELARA_LOG_LEVEL=DEBUG ELARA_LOG_NAMESPACE=TypeInfer elara --run
```

### Minimal Output (Errors Only)

```bash
ELARA_LOG_LEVEL=ERROR elara --run
```

## Benefits

### For Developers

- **Easier Debugging**: Quickly find where issues occur with source locations
- **Better Organization**: Namespaces group related logs together
- **Focused Investigation**: Filter to specific components when debugging
- **Clear Priorities**: Log levels show what's important vs. informational

### For Users

- **Less Noise**: Control verbosity to see only what matters
- **Better Performance**: Filtered logs mean less I/O and faster compilation
- **Troubleshooting**: Share logs at appropriate levels for bug reports

### For Maintainers

- **Consistent Style**: Standardized logging across the codebase
- **Extensible**: Easy to add more log levels or features in the future
- **Documented**: Comprehensive docs and examples for contributors

## Performance Impact

When logging is disabled (via `ignoreStructuredDebug`), there is essentially zero runtime overhead as log messages are not evaluated. When enabled, the system is designed to be efficient:

- Filtering happens early (before formatting)
- Timestamps are only computed if enabled
- Source locations use GHC's built-in `HasCallStack` (zero runtime cost)

## Migration Path

Existing code requires no changes. To benefit from new features:

1. **Add appropriate log levels** to existing `debug` calls:
   ```haskell
   debug "Processing..." → logDebug "Processing..."
   debug "Success!" → logInfo "Success!"
   ```

2. **Add namespaces** for better organization:
   ```haskell
   logDebugNS ["TypeInfer"] "Generating constraints"
   ```

3. **Use environment variables** for runtime control without code changes

## Implementation Details

- **Location**: `src/Elara/Logging.hs`
- **Lines Added**: ~180 lines (new features)
- **Lines Changed**: ~50 lines (integration)
- **Backward Compatibility**: 100%
- **Tests**: `test/Logging.hs`
- **Documentation**: `docs/logging.md`, `docs/README_LOGGING.md`, `docs/examples/LoggingExamples.hs`

## Future Enhancements

Potential future improvements:

1. **Log Rotation**: Automatic log file management
2. **Structured Logs**: JSON output for machine parsing
3. **Performance Metrics**: Timing information in logs
4. **Remote Logging**: Send logs to external services
5. **Log Replay**: Record and replay log sessions for debugging

## Conclusion

The improved structured logging system provides powerful debugging and monitoring capabilities while maintaining full backward compatibility. It makes the Elara compiler easier to develop, debug, and maintain.

For more information, see:
- [Complete Documentation](logging.md)
- [Quick Start Guide](README_LOGGING.md)
- [Code Examples](examples/LoggingExamples.hs)
