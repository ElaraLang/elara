# Flags

## Dumping 
Elara supports the following flags for debugging and development:
- `--dump-lexed` - Dumps a list of tokens after lexing
- `--dump-parsed` - Dumps the Frontend AST after parsing
- `--dump-desugared` - Dumps the Desugared AST after desugaring
- `--dump-renamed` - Dumps the Renamed AST after renaming
- `--dump-shunted` - Dumps the Shunted AST after shunting
- `--dump-typed` - Dumps the Typed AST after type checking
- `--dump-core` - Dumps the Core AST after converting to Core

These will all dump to the generated `build/` directory, matching the module name where possible.

## Execution
Elara supports the following flags for controlling execution:
- `--run` - Runs the program in interpreted mode
<div class="warning">
**Important Caveat:** Currently the `--run` flag must be used to run the program in interpreted mode. You will receive a warning if you do not use this flag and no code will be run. The `just` commands above already include this flag.
</div>