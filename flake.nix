{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-root.url = "github:srid/flake-root";
    just-flake.url = "github:juspay/just-flake";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    h2jvm.url = "github:ElaraLang/h2jvm";

    diagnose.url = "github:bristermitten/diagnose";
    diagnose.flake = false;

    all-cabal-hashes.url = "github:commercialhaskell/all-cabal-hashes/hackage";
    all-cabal-hashes.flake = false;
  };

  outputs = inputs@{ self, pre-commit-hooks, nixpkgs, ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {

      systems = import inputs.systems;

      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.just-flake.flakeModule
      ];

      perSystem = { self', lib, system, config, pkgs, ... }: {

        haskellProjects.default = {

          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell

          basePackages = pkgs.haskell.packages.ghc910.override {
            all-cabal-hashes = inputs.all-cabal-hashes; # we need this so that the later x.source = y sections work
          };


          packages = {
            h2jvm.source = inputs.h2jvm;
            diagnose.source = inputs.diagnose;

            fourmolu.source = "0.18.0.0";
            hlint.source = "3.10";
            ghc-lib-parser.source = "9.12.1.20250314";
            ghc-lib-parser-ex.source = "9.12.0.0";
            ormolu.source = "0.8.0.0";
            Cabal-syntax.source = "3.14.1.0";

            polysemy-test.source = "0.10.0.1";

            polysemy-time.source = "0.7.0.1";
            polysemy-resume.source = "0.9.0.1";
            polysemy-conc.source = "0.14.1.1";
          };

          settings = {
            ghc-lib-parser.buildFromSdist = true;
            fourmolu.check = false;
            fourmolu.jailbreak = true;
            polysemy-log.jailbreak = true;
            incipit-base.jailbreak = true;
            incipit-core.jailbreak = true;

            diagnose = {
              cabalFlags.megaparsec-compat = true;
              jailbreak = true;
            };

            type-errors = {
              extraSetupDepends = [ pkgs.haskellPackages.doctest ];
              extraBuildDepends = [ pkgs.haskellPackages.doctest ];
              check = false;
            };

            h2jvm = {
              # Skip the tests due to conflicting base version
              check = false;
            };

            ghcid = {
              separateBinOutput = false;
            };

            crypton-x509 = { check = false; };

          };

          devShell = {
            hlsCheck.enable = false;
          };
        };


        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              # treefmt.package = config.treefmt.build.wrapper;
              treefmt.enable = true;
            };
          };
        };

        just-flake.features = {
          treefmt.enable = true;
        };



        packages.default = self'.packages.elara;

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.just-flake.outputs.devShell
          ];
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          nativeBuildInputs = [ pkgs.just pkgs.convco pkgs.treefmt ];

          buildInputs =
            let
              stack-wrapped = pkgs.symlinkJoin
                {
                  name = "stack"; # will be available as the usual `stack` in terminal
                  paths = [ pkgs.stack ];
                  buildInputs = [ pkgs.makeWrapper ];
                  postBuild = ''
                    wrapProgram $out/bin/stack \
                      --add-flags "\
                        --nix \
                        --system-ghc \
                        --no-install-ghc \
                      "
                  '';
                };
            in
            [
              stack-wrapped
              pkgs.haskellPackages.haskell-debug-adapter
              pkgs.haskellPackages.ghci-dap
              pkgs.haskellPackages.hpack
              pkgs.git-cliff
            ];
        };

      };
    };
}
