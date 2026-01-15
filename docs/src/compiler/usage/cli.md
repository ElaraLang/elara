# CLI

Elara has a command line interface (CLI) to build and run Elara programs. The CLI is invoked using the `elara` command followed by various options and flags.

<div class="warning">
<strong>With Cabal</strong> When running with Cabal in Development Mode, you should instead use <tt>cabal run elara -- [options]</tt> to interact with the CLI.
</div>

## Subcommands

- `build`
Compiles the Elara source code.
<div class="warning">
This command is currently useless and will do nothing.
</div>

- `run`
  Compiles and runs the Elara source code

By default, this runs in interpreted mode. You can use `--target jvm` to compile to JVM bytecode instead.
For example, `elara run source.elr --target jvm`

## Dumping

The compiler can intermediate representations of the code during compilation for debugging and development purposes.

You can enable this using the `--dump` flag followed by a comma-separated list of targets to dump. The available dump targets are:

| Dump Target | Description                                                                                                                                    |
| ----------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| `lexed`     | Dumps a list of tokens after lexing                                                                                                            |
| `parsed`    | Dumps the Frontend AST after parsing                                                                                                           |
| `desugared` | Dumps the Desugared AST after desugaring                                                                                                       |
| `renamed`   | Dumps the Renamed AST after renaming                                                                                                           |
| `shunted`   | Dumps the Shunted AST after shunting                                                                                                           |
| `typed`     | Dumps the Typed AST after type checking                                                                                                        |
| `core`      | Dumps all stages of the Core language                                                                                                          |
| `ir`        | Dumps the JVM bytecode IR representation (only works when running with `--target jvm`)                                                         |
| `jvm`       | Dumps the generated JVM bytecode representation from [H2JVM](https://github.com/ElaraLang/H2JVM) (only works when running with `--target jvm`) |

All dumps are written to the `build/` directory in the current working directory.
