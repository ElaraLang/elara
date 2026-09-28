default:
    @just --list

# Compile all the Java Stdlib
stdlib:
    javac $(/usr/bin/find jvm-stdlib -name "*.java")

# Run the project tests
test: stdlib
    cabal test --ghc-options="-O0" elara-test

# Run the project's source.elr file
run file='source.elr' target='interp': stdlib
   cabal run --ghc-options="-O0"  elara -- run {{file}} --target {{target}}


