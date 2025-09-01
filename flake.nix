{
  description = "Elara Programming Language";
  inputs = {
    hix = {
      url = "github:tek/hix?ref=0.9.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    git-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    h2jvm.url = "github:ElaraLang/h2jvm";
    diagnose = {
      url = "github:bristermitten/diagnose";
      flake = false;
    };

  };

  outputs =
    {
      self,
      hix,
      h2jvm,
      flake-parts,
      git-hooks-nix,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } (
      { ... }:
      {
        debug = true;
        systems = import inputs.systems;
        imports = [
          inputs.treefmt-nix.flakeModule
          inputs.git-hooks-nix.flakeModule
        ];
        perSystem =
          {
            config,
            pkgs,
            lib,
            ...
          }:
          {
            treefmt = {
              programs.actionlint.enable = true;
              programs.nixfmt.enable = true;
              programs.fourmolu.enable = builtins.break true;
              programs.fourmolu.package = pkgs.haskell.packages.ghc912.fourmolu;
              programs.fourmolu.ghcOpts = [ ];
              programs.hlint.enable = true;
              programs.yamlfmt.enable = true;
              programs.toml-sort.enable = true;
            };
            pre-commit.settings.hooks.treefmt.enable = true;
          };
        flake = hix.lib.flake (
          { config, ... }:
          {
            # ghcVersions = ["ghc98" "ghc910" "ghc912"];
            compiler = "ghc912";
            outputs.devShells = {
              # extending the default devshell to add the pre-commit hooks and some other nice things
              default = config.pkgs.mkShell {
                inputsFrom = [
                  config.outputs.devShells.dev # the devshell that hix provides
                  self.allSystems.${config.system}.pre-commit.devShell # the pre-commit devshell
                ];
                buildInputs = with config.pkgs; [
                  just
                  convco
                  git-cliff
                  nixfmt
                ];
              };
            };

            overrides =
              {
                source,
                hackage,
                enable,
                notest,
                unbreak,
                jailbreak,
                ...
              }:
              {
                h2jvm = source.root h2jvm;
                unix = enable "os-string";
                directory = enable "os-string";
                diagnose = enable "megaparsec-compat" (source.root inputs.diagnose);
                incipit-base = jailbreak;
                polysemy-conc = jailbreak;
                polysemy-test = unbreak (jailbreak);
                polysemy-time = jailbreak;
                polysemy-resume = jailbreak;
                polysemy-log = jailbreak;
                incipit-core = jailbreak;
                ghc-tcplugins-extra = hackage "0.5" "sha256-mOzdicJevaXZdZS4/RA1hU3CWJXMFwMUfmEH3YxX4Q8=";

                optics-core = notest; # test fails on ghc 9.12
                optics = notest; # test fails on ghc 9.12
                generic-optics = notest; # test fails on ghc 9.12

              };
            packages = {
              elara = {
                buildInputs = pkgs: [ pkgs.alex ];
                src = ./.;
                description = "See README for more info";
                cabal = {
                  author = "Alexander Wood";
                  build-type = "Simple";
                  copyright = "2022 Alexander Wood";
                  license = "MIT";
                  license-file = "LICENSE";
                  version = "0.1.0";
                  meta = {
                    maintainer = "Alexander Wood <alexljwood24@hotmail.co.uk>";
                    homepage = "https://github.com/ElaraLang/elara#readme";
                    synopsis = "See README for more info";
                  };
                  language = "GHC2024";
                  prelude = {
                    enable = true;
                    package = "relude";
                    module = "Prelude";
                  };
                  default-extensions = [
                    "OverloadedStrings"
                    "OverloadedRecordDot"
                    "TypeFamilies"
                    "LambdaCase"
                    "ImportQualifiedPost"
                    "DeriveDataTypeable"
                    "DataKinds"
                    "DeriveFunctor"
                    "TypeApplications"
                    "PartialTypeSignatures"
                  ];
                  dependencies = [
                    "aeson"
                    "algebraic-graphs"
                    "array"
                    "binary"
                    "bytestring"
                    "containers"
                    "diagnose"
                    "directory"
                    "filepath"
                    "generic-optics"
                    "h2jvm"
                    "hashable"
                    "lens"
                    "matrix"
                    "megaparsec"
                    "mtl"
                    "optics"
                    "parser-combinators"
                    "polysemy"
                    "polysemy-log"
                    "polysemy-plugin"
                    "polysemy-time"
                    "pretty-simple"
                    "prettyprinter"
                    "prettyprinter-ansi-terminal"
                    "process"
                    "relude"
                    "safe-exceptions"
                    "stringsearch"
                    "terminal-size"
                    "text-metrics"
                    "utf8-string"
                  ];
                  ghc-options = [
                    "-W"
                    "-Wno-name-shadowing"
                    "-Wno-partial-type-signatures"
                    "-Widentities"
                    "-optP-Wno-nonportable-include-path"
                    "-fdefer-typed-holes"
                    "-fno-show-valid-hole-fits"
                    "-fplugin=Polysemy.Plugin"
                    "-fwrite-ide-info"
                    "-hiedir=.hie"
                    "-O0"
                  ];
                };
                library = {
                  enable = true;
                  source-dirs = "src";

                  component = {
                    build-tools = [
                      "alex"
                    ];
                  };
                };
                executables.elara = {
                  source-dirs = "app";

                };
                tests.elara-test = {
                  main = "Spec.hs";
                  default-extensions = [
                    "QuasiQuotes"
                  ];
                  dependencies = [
                    "HUnit"
                    "QuickCheck"
                    "hedgehog"
                    "sydtest"
                    "sydtest-hedgehog"
                    "template-haskell"
                    "neat-interpolation"
                    "hspec-megaparsec"
                  ];
                  source-dirs = "test";

                  component = {
                    other-modules = [
                      "Arbitrary.AST"
                      "Arbitrary.Literals"
                      "Arbitrary.Names"
                      "Arbitrary.Type"
                      "Boilerplate"
                      "Common"
                      "HedgehogSyd"
                      "Infer"
                      "Infer.Unify"
                      "Lex"
                      "Lex.Common"
                      "Lex.Indents"
                      "Orphans"
                      "Parse"
                      "Parse.Common"
                      "Parse.Expressions"
                      "Parse.Patterns"
                      "Region"
                      "Shunt"
                    ];
                  };
                };
              };
            };
          }
        );
      }
    );
}
