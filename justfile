default:
    @just --list

# Run the project tests with ghcid auto-recompile
test:
    cabal test --ghc-options="-O0" --enable-profiling elara-test

# Run the project with ghcid auto-recompile
run:
   cabal run --ghc-options="-O0" --enable-profiling elara -- --run
