# Flags

Most flags can be passed either as command line arguments or as environment variables. For example, the flag `--dump-lexed` can also be set by setting the environment variable `ELARA_DUMP_LEXED=1`. The `1` can be replaced with any non-empty string to enable the flag.

## Dumping

Elara supports the following flags for debugging and development, with corresponding environment variables:

| Command Line Flag  | Environment Variable   | Description                                 |
| ------------------ | ---------------------- | ------------------------------------------- |
| `--dump-lexed`     | `ELARA_DUMP_LEXED`     | Dumps a list of tokens after lexing         |
| `--dump-parsed`    | `ELARA_DUMP_PARSED`    | Dumps the Frontend AST after parsing        |
| `--dump-desugared` | `ELARA_DUMP_DESUGARED` | Dumps the Desugared AST after desugaring    |
| `--dump-renamed`   | `ELARA_DUMP_RENAMED`   | Dumps the Renamed AST after renaming        |
| `--dump-shunted`   | `ELARA_DUMP_SHUNTED`   | Dumps the Shunted AST after shunting        |
| `--dump-typed`     | `ELARA_DUMP_TYPED`     | Dumps the Typed AST after type checking     |
| `--dump-core`      | `ELARA_DUMP_CORE`      | Dumps the Core AST after converting to Core |

These will all dump to the generated `build/` directory, matching the module name where possible.

## Execution

Elara supports the following flags for controlling execution:

- `--run` / `ELARA_RUN` - Runs the program in interpreted mode
<div class="warning">
<strong>Important Caveat</strong> Currently the <tt>--run</tt> flag must be used. You will receive a warning if you do not use this flag and no code will be processed. The <tt>just</tt> commands provided by the <a href="/compiler/usage/setup.md">development environment</a> already include this flag.
</div>
