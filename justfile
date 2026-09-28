default:
    @just --list

# Run the project tests
test:
    cabal test --ghc-options="-O0" elara-test

# Run the project's source.elr file
run file='source.elr' target='interp':
   cabal run --ghc-options="-O0"  elara -- run {{file}} --target {{target}}


# Compile all the Java Stdlib
stdlib:
    javac $(/usr/bin/find jvm-stdlib -name "*.java")