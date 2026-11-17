ghc_flags := '--ghc-options=-O0'

[private]
default:
    @just --list

clean:
    rm -rf build/

# Build everything
build:
    cabal build {{ ghc_flags }} all

# Format the project
fmt:
    nix fmt

# Start a REPL for the elara library
repl:
    cabal repl {{ ghc_flags }} elara

# Run the project tests, optionally filtering with args (e.g. `just test -p Parse`)
test *args:
    cabal test {{ ghc_flags }} elara-test --test-options='{{ args }}'

# Run an Elara file, e.g. `just run examples/json_parser.elr --target jvm`
[arg('target', long='target', pattern='interp|jvm')]
run file target='interp':
    cabal run {{ ghc_flags }} elara -- run {{ file }} --target {{ target }}
