ghc_flags := '--ghc-options=-O0'

[private]
default:
    @just --list

# Compile the Java Stdlib, skipping it when no .java file changed since the last build
stdlib:
    #!/usr/bin/env bash
    set -euo pipefail
    stamp=build/stdlib.stamp
    if [ -f "$stamp" ] && [ -z "$(/usr/bin/find jvm-stdlib -name '*.java' -newer "$stamp")" ]; then
        exit 0
    fi
    javac $(/usr/bin/find jvm-stdlib -name '*.java')
    mkdir -p build
    touch "$stamp"

clean:
    /usr/bin/find jvm-stdlib -name "*.class" -delete
    rm -f build/

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
test *args: stdlib
    cabal test {{ ghc_flags }} elara-test --test-options='{{ args }}'

# Run the project's source.elr file
[arg('file', long='file')]
[arg('target', long='target', pattern='interp|jvm')]
run file='source.elr' target='interp':
    {{ if target == 'jvm' { 'just stdlib' } else { '' } }} # only build java stdlib if required
    cabal run {{ ghc_flags }} elara -- run {{ file }} --target {{ target }}
