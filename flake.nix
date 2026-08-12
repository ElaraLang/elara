{
  description = "Elara Programming Language";
  inputs = {
    hix = {
      url = "github:tek/hix";
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

    # hackage latest version is broken but this github fork works
    dependent-hashmap = {
      url = "github:ElaraLang/dependent-hashmap";
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
              programs.fourmolu.enable = true;
              programs.fourmolu.package = pkgs.haskell.packages.ghc912.fourmolu;
              programs.hlint.enable = true;
              programs.hlint.package = pkgs.haskell.packages.ghc912.hlint;
              programs.yamlfmt.enable = true;
              programs.toml-sort.enable = true;
            };
            pre-commit.settings.hooks.treefmt.enable = true;
            devShells.pre-commit = config.pre-commit.devShell;
          };
        flake =
          let
            hixFlake = hix.lib.flake (
              { config, ... }:
              let
                depOverrides =
                  {
                    source,
                    hackage,
                    enable,
                    notest,
                    force,
                    unbreak,
                    jailbreak,
                    ...
                  }:
                  {
                    h2jvm = jailbreak (notest (source.root h2jvm));
                    unix = enable "os-string";
                    directory = enable "os-string";
                    diagnose = enable "megaparsec-compat" (source.root inputs.diagnose);
                    incipit-base = jailbreak;
                    incipit-core = jailbreak;
                    ghc-tcplugins-extra = hackage "0.5" "sha256-mOzdicJevaXZdZS4/RA1hU3CWJXMFwMUfmEH3YxX4Q8=";

                    # many things don't yet work on ghc 9.14 without jailbreakng
                    kind-generics = jailbreak;
                    kind-generics-th = jailbreak;
                    svg-builder = jailbreak;
                    terminfo = jailbreak;
                    tasty-hspec = jailbreak;
                    rebase = jailbreak;
                    dependent-sum-template = jailbreak;
                    constraints-extras = jailbreak;

                    # 0.7 in package set breaks against containers 0.8 from ghc 9.14
                    algebraic-graphs = hackage "0.8" "sha256-lkVkS7WqNMC07MZfJ9aBFWPJTTyg1jbp3BWHOJMnL2I=";

                    relude = notest; # doctest fails for some reason

                    optics-core = notest; # test fails on ghc 9.12
                    optics = notest; # test fails on ghc 9.12

                    # quite a few things are outdated in nixpkgs' pin
                    generic-optics = notest (hackage "2.3.0.0" "sha256-3xDJDQAlIYg+Xn2j8qgTyrVSe1/C+Ad1HHVImHPTM50=");
                    generic-lens-core = hackage "2.3.0.0" "sha256-Abntgf3UMhQed5gOc6sDoVilMc0FRRCh8VJCeoQfNRY=";
                    autodocodec-schema = hackage "0.2.0.2" "sha256-hnpqfkthYWmFP3GSk5jAuxTVv/nf46M8wfvoX+S6MUs=";
                    autodocodec-nix = hackage "0.1.0.2" "sha256-c1aQ8JNZGqFx+YiIogB7bXTqWkXFfrGCfIJhWlOCEXY=";
                    autodocodec-yaml = hackage "0.4.0.3" "sha256-uMiorgI9FRt2YxgUIB4R+tpMkFyauinCl2xydyPwoB8=";

                    aeson = hackage "2.3.1.0" "sha256-vWVNPb/kzXR92XmS8/TwfXWfhhhASjx6nRhcWj1aMmg=";
                    autodocodec = hackage "0.6.0.0" "sha256-sE9+yKdJXIUAdJP/J1yrYS3d5fj7ESMYQAFLu8liXe8=";
                    opt-env-conf = hackage "0.15.0.2" "sha256-uWUSQxspveZMrv2DaBnoXvFtBphvpzQ5p0eyNxHPOKA=";
                    sydtest = hackage "0.23.0.0" "sha256-bfpNiMF62Vj4pK2AKJ/yOquDP/bpI8Pjn/mSQVHJQXk=";

                    dependent-hashmap = source.root inputs.dependent-hashmap;

                    effectful = jailbreak (hackage "2.6.1.0" "sha256-krNjGxqdbmFpt1g3anTd5ajGtYnyvGaG+AiDLfJN8No=");
                    effectful-core = jailbreak;
                    effectful-plugin = jailbreak;
                    co-log-effectful = jailbreak (unbreak);

                    boring = jailbreak;
                    some = jailbreak;
                    hie-compat = jailbreak;
                    ghcide = jailbreak;
                    opentelemetry = jailbreak;
                  };
              in
              {
                compiler = "ghc9141";
                systems = import inputs.systems;
                compat.enable = false;
                envs.dev.ghcid.enable = false;
                # hix's HLS build can't be patched (overrides don't seem to reach transitive deps like cabal-add)
                # so we disable it and inject a manually-patched HLS in buildInputs below
                envs.dev.hls.enable = false;

                managed = {
                  enable = true;
                  latest.compiler = "ghc9141"; # use 9.14 for bounds testing
                  lower.enable = true;
                  lower.compiler = "ghc9141"; # ditto for lower bounds testing

                  forceBounds.base.upper = "4.23"; # default doesn't work idk exactly why

                  envs = {
                    solverOverrides = depOverrides;
                    verbatim.overrides = depOverrides;
                  };
                };
                outputs.devShells = {
                  # extending the default devshell to add the pre-commit hooks and some other nice things
                  default = config.pkgs.mkShell {
                    inputsFrom = [
                      config.outputs.devShells.dev # the devshell that hix provides
                      self.devShells.${config.system}.pre-commit # the pre-commit devshell
                    ];
                    buildInputs = with config.pkgs; [
                      just
                      convco
                      git-cliff
                      nixfmt

                      mdbook
                      mdbook-d2
                      mdbook-variables
                      d2
                      nixd

                      # lots of hls tests break from nix sandboxing
                      # let's just disable them all hehe
                      (
                        let
                          rawPkgs = import inputs.nixpkgs { system = config.system; };
                          hlsPkgs = rawPkgs.haskell.packages.ghc9141.override {
                            overrides = hfinal: hprev: {
                              # cabal-add test fails due to nix sandboxing
                              cabal-add = rawPkgs.haskell.lib.dontCheck hprev.cabal-add;
                              # ditto
                              fourmolu = rawPkgs.haskell.lib.dontCheck hprev.fourmolu;
                              # dependency bounds bad
                              haskell-language-server = rawPkgs.haskell.lib.dontCheck (
                                rawPkgs.haskell.lib.doJailbreak hprev.haskell-language-server
                              );
                            };
                          };
                        in
                        hlsPkgs.haskell-language-server
                      )
                    ];
                  };
                };

                overrides = depOverrides;
                packages = {
                  elara = {
                    buildInputs = pkgs: [
                      pkgs.alex
                    ];
                    src = ./.;
                    description = "See README for more info";

                    override =
                      { overrideAttrs, pkgs, ... }:
                      drv:
                      overrideAttrs (old: {
                        unfilteredSrc = ./.;

                        nativeCheckInputs = (old.nativeCheckInputs or [ ]) ++ [ pkgs.jdk ]; # jdk is only needed for checks, breaks static building otherwise

                        preCheck = ''
                          ${old.preCheck or ""}
                          echo "Compiling Java standard library..."

                          JAVA_FILES=$(${pkgs.findutils}/bin/find jvm-stdlib -name "*.java")

                          javac $JAVA_FILES
                        '';
                      }) drv;

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
                        github = "ElaraLang/elara";

                        extra-source-files = [
                          "jvm-stdlib/**/*.java"
                        ];
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
                        "co-log-core"
                        "co-log-effectful"
                        "containers"
                        "dependent-sum-template"
                        "diagnose"
                        "directory"
                        "dependent-hashmap"
                        "effectful >= 2.6.1.0"
                        "effectful-core"
                        "effectful-plugin"
                        "effectful-th"
                        "filepath"
                        "generic-optics"
                        "h2jvm"
                        "hashable"
                        "kind-generics-th"
                        "lens"
                        "lifted-base"
                        "matrix"
                        "megaparsec"
                        "mtl"
                        "optics"
                        "parser-combinators"
                        "pretty-simple"
                        "prettyprinter"
                        "prettyprinter-ansi-terminal"
                        "process"
                        "relude"
                        "safe-exceptions"
                        "some"
                        "stringsearch"
                        "template-haskell"
                        "terminal-size"
                        "text-metrics"
                        "time"
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
                        "-fplugin=Effectful.Plugin"
                        "-fwrite-ide-info"
                        "-hiedir=.hie"
                        "-O0"
                        "-threaded"
                        "-rtsopts"
                      ];
                    };
                    library = {
                      enable = true;
                      source-dirs = "src";

                      dependencies = [
                        "witch"
                      ];

                      component = {
                        build-tools = [
                          "alex"
                        ];
                      };
                    };
                    executables.elara = {
                      source-dirs = "app";
                      dependencies = [
                        "autodocodec"
                        "opt-env-conf"
                      ];
                      component.other-modules = [ "Paths_elara" ];
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
                    };
                  };
                };
              }
            );
          in
          # Strip non-standard hix outputs that fail the Nix flake schema
          # validator when merged by flake-parts, and remove gen-overrides app
          # which evaluates cabal2nix for all systems and fails cross-platform.
          builtins.removeAttrs hixFlake [
            "debug"
            "lib"
            "overrides"
          ]
          // {
            apps = builtins.mapAttrs (_system: apps: builtins.removeAttrs apps [ "gen-overrides" ]) (
              hixFlake.apps or { }
            );
          };
      }
    );
}
