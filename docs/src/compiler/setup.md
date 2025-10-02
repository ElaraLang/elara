# Compiler Setup

This section provides instructions on how to set up the Elara compiler in your development environment.

The recommended workflow to build is with Nix, as this will ensure you have the correct versions of all dependencies and tools.
If you don't have / want Nix, you _should_ be able to get away with a manually installed GHC 9.12.2 and Cabal

### Building with Nix

1. Run `nix build` to build
2. You should be able to access Elara the executable from `./result/bin/elara`

### Hacking with Nix

1. Run `nix develop` to enter a shell with all dependencies
2. Use `just run` to run in development mode (with an interpreter)
3. To run unit tests, run `just test`

### Building without Nix

1. Run `cabal build` to build

### Running without Nix

1. Run `cabal run` to run
2. Run `cabal test` to run unit tests