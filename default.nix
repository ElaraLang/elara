{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  haskellDeps = ps: with ps; [
                    haskell-language-server
                    fourmolu
                    cabal-install
  ];
  haskellEnv = haskell.packages.ghc944.ghcWithPackages haskellDeps;
in mkShell {
  buildInputs = [
    haskellEnv
    haskellPackages.cabal-install
  ];
}