# CLI

Elara has a command line interface (CLI) to build and run Elara programs. The CLI is invoked using the `elara` command followed by various options and flags.

> [!NOTE]
> <strong>With Cabal</strong> When running with Cabal in Development Mode, you should instead use <tt>cabal run elara -- [options]</tt> to interact with the CLI.

## Subcommands

- `build`
Compiles the Elara source code.

> [!WARNING]
> This command is currently useless and will do nothing.


- `run`
  Compiles and runs the Elara source code

By default, this runs in interpreted mode. You can use `--target jvm` to compile to JVM bytecode instead.
For example, `elara run source.elr --target jvm`. This needs Java 8 or newer on your `PATH` to run the compiled bytecode.

- `stdlib path`
  Prints the directory the standard library is loaded from.

- `stdlib eject [DIR]`
  Copies the bundled standard library into a new directory (`stdlib` by default), so you can customise it.
  
  > [!NOTE]
  > This is unlikely to be useful for the average user, but helpful for testing if modifying the standard library!

## Standard library

The standard library is bundled into the `elara` executable and unpacked into your `$XDG_CACHE` (or MacOS/Windows equivalents) directory on first use, so you can run `elara` from any directory.
To use a different copy, for example one made with `elara stdlib eject`, set one of these:

- the `--stdlib-dir DIR` flag
- the `ELARA_STDLIB_DIR` environment variable
- `stdlib-dir` in the config file

The native Java part of the standard library is bundled too. It can't be customised (for now?), and is written into the `build` directory alongside your compiled classes, so `java -cp build Main` should just work!

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
