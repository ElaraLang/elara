# Structured Debug and Logging System

The Elara compiler includes a structured debug and logging system built on the `StructuredDebug` effect. This system provides hierarchical, namespaced logging with multiple log levels, timestamps, and source location tracking.

## Features

- **Multiple Log Levels**: Debug, Info, Warning, and Error
- **Hierarchical Indentation**: Nested log messages are automatically indented
- **Namespace Support**: Organize logs by component or module
- **Source Location Tracking**: Automatically includes file and line information
- **Timestamps**: Optional timestamps for each log message
- **Configurable Filtering**: Filter by log level or namespace
- **Environment Variable Configuration**: Control logging behavior without code changes

## Configuration

Configure logging behavior using environment variables:

| Variable | Description | Values | Default |
|----------|-------------|--------|---------|
| `ELARA_DEBUG` | Enable debug logging | any value | not set |
| `ELARA_LOG_LEVEL` | Minimum log level | DEBUG, INFO, WARN, ERROR | INFO (or DEBUG if ELARA_DEBUG is set) |
| `ELARA_LOG_TIMESTAMPS` | Show timestamps | true, false | **false** |
| `ELARA_LOG_SOURCE_LOC` | Show source location | true, false | **false** |
| `ELARA_LOG_NAMESPACE` | Filter by namespace | dot-separated namespace | not set |

**Note**: Timestamps and source locations are disabled by default for cleaner output during simple debugging. Enable them when you need detailed diagnostics.

### Examples

```bash
# Enable debug logging (clean output)
ELARA_DEBUG=1 elara --run

# Show only warnings and errors
ELARA_LOG_LEVEL=WARN elara --run

# Enable timestamps for detailed diagnostics
ELARA_LOG_TIMESTAMPS=true elara --run

# Enable source locations for debugging
ELARA_LOG_SOURCE_LOC=true elara --run

# Full diagnostic mode with all metadata
ELARA_LOG_TIMESTAMPS=true ELARA_LOG_SOURCE_LOC=true elara --run

# Filter to only show type inference logs
ELARA_LOG_NAMESPACE=TypeInfer elara --run
```

## Usage

### Basic Logging

The system provides functions for each log level:

```haskell
import Elara.Logging

-- Debug level (most verbose)
logDebug "Starting type inference"

-- Info level (general information)
logInfo "Module compiled successfully"

-- Warning level (potential issues)
logWarning "Deprecated function used"

-- Error level (serious problems)
logError "Type mismatch detected"
```

### Namespaced Logging

Organize logs by component using namespaces:

```haskell
-- Single namespace
logDebugNS ["TypeInfer"] "Generating constraints"

-- Nested namespace
logInfoNS ["TypeInfer", "Unification"] "Unified types successfully"
```

### Scoped Logging

Log entry and exit of a code section with automatic indentation:

```haskell
-- Simple scope
result <- logDebugWith "Type checking expression" $ do
    -- code here is indented in logs
    checkTypes expr

-- Scoped with namespace
result <- logInfoWithNS ["Parser"] "Parsing module" $ do
    parseModule source
```

### Legacy Functions

Backward-compatible functions (map to Debug level):

```haskell
-- Simple debug message
debug "Processing..."

-- Debug with scope
result <- debugWith "Analyzing" $ analyze code

-- Debug with result logging
result <- debugWithResult "Computing" $ compute value

-- Namespaced versions
debugNS ["Module", "Submodule"] "Message"
debugWithNS ["Module"] "Message" action
```

## Example Output

**Default output** (clean and readable for simple debugging):

```
[INFO] Starting compilation
[DEBUG] [TypeInfer] Generating constraints
│ [DEBUG] [TypeInfer] Processing expression
│ │ [DEBUG] [TypeInfer.Unification] Unifying types
│ │ [DEBUG] [TypeInfer.Unification] Result: Int -> Bool
[INFO] Compilation successful
```

**With timestamps enabled** (`ELARA_LOG_TIMESTAMPS=true`):

```
2025-12-30 13:07:56 [INFO] Starting compilation
2025-12-30 13:07:56 [DEBUG] [TypeInfer] Generating constraints
│ 2025-12-30 13:07:56 [DEBUG] [TypeInfer] Processing expression
│ │ 2025-12-30 13:07:56 [DEBUG] [TypeInfer.Unification] Unifying types
│ │ 2025-12-30 13:07:56 [DEBUG] [TypeInfer.Unification] Result: Int -> Bool
2025-12-30 13:07:56 [INFO] Compilation successful
```

**With all diagnostics enabled** (`ELARA_LOG_TIMESTAMPS=true ELARA_LOG_SOURCE_LOC=true`):

```
2025-12-30 13:07:56 [INFO] Main.hs:142 Starting compilation
2025-12-30 13:07:56 [DEBUG] TypeInfer.hs:45 [TypeInfer] Generating constraints
│ 2025-12-30 13:07:56 [DEBUG] TypeInfer.hs:98 [TypeInfer] Processing expression
│ │ 2025-12-30 13:07:56 [DEBUG] TypeInfer.hs:123 [TypeInfer.Unification] Unifying types
│ │ 2025-12-30 13:07:56 [DEBUG] TypeInfer.hs:125 [TypeInfer.Unification] Result: Int -> Bool
2025-12-30 13:07:56 [INFO] Main.hs:180 Compilation successful
```
```

## Implementation Details

### Log Levels

Log levels are ordered by severity:
- `Debug`: Detailed diagnostic information
- `Info`: General informational messages
- `Warning`: Warning messages for potential issues
- `Error`: Error messages for serious problems

Messages are only displayed if their level meets or exceeds the configured minimum level.

### Source Location

The system uses `HasCallStack` to automatically capture source file and line information for each log message. This can be disabled via the `ELARA_LOG_SOURCE_LOC` environment variable.

### Namespace Filtering

When `ELARA_LOG_NAMESPACE` is set, only messages whose namespace starts with the configured prefix are displayed. For example:

- `ELARA_LOG_NAMESPACE=TypeInfer` matches: `["TypeInfer"]`, `["TypeInfer", "Unification"]`
- Does not match: `["Parser"]`, `["Core"]`

### Performance

When logging is disabled via `ignoreStructuredDebug`, log messages have minimal runtime overhead as they are not evaluated.

## Best Practices

1. **Use appropriate log levels**:
   - `Debug`: Detailed trace of program flow
   - `Info`: Major milestones and user-facing information
   - `Warning`: Recoverable issues or deprecated usage
   - `Error`: Failures and exceptional conditions

2. **Use namespaces for organization**:
   - Group related logs by module or component
   - Use hierarchical namespaces for subsystems

3. **Provide context in messages**:
   - Include relevant variable names and values
   - Use the Pretty type class for consistent formatting

4. **Use scoped logging for phases**:
   - Use `logDebugWith` / `logInfoWith` for major phases
   - Indentation clearly shows the code structure

5. **Keep messages concise**:
   - Log messages should be easy to scan
   - Use structured data (Pretty instances) for complex output

## Migration from Old System

The new system is fully backward compatible. Existing code using `debug`, `debugWith`, etc. will continue to work and will be treated as Debug-level logs.

To benefit from the new features:

1. Replace `debug` with `logDebug`, `logInfo`, etc. based on importance
2. Add namespaces to categorize logs: `logDebugNS ["Module"]`
3. Use environment variables to control logging without code changes

## See Also

- `src/Elara/Logging.hs` - Implementation
- `app/Main.hs` - Integration with the main compiler
- Type inference modules - Examples of extensive logging usage
