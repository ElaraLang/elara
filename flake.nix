{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    just-flake.url = "github:juspay/just-flake";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    h2jvm.url = "github:ElaraLang/h2jvm";

    diagnose.url = "github:knightzmc/diagnose";
    diagnose.flake = false;

    megaparsec.url = "github:mrkkrp/megaparsec";
    megaparsec.flake = true;
  };

  outputs = inputs@{ self, pre-commit-hooks, nixpkgs, ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {

      systems = import inputs.systems;

      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.just-flake.flakeModule
      ];

      perSystem = { self', lib, system, config, pkgs, ... }: {

        haskellProjects.default = {

          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell

          basePackages = pkgs.haskell.packages.ghc965;


          packages = {
            h2jvm.source = inputs.h2jvm;
            diagnose.source = inputs.diagnose;
            megaparsec.source = inputs.megaparsec;
            polysemy-test.source = "0.9.0.0";
          };

          settings = {

            fourmolu.check = false;
            polysemy-test.jailbreak = true;
            polysemy-conc.jailbreak = true;
            polysemy-log.jailbreak = true;

            diagnose = {
              extraBuildDepends = [
                pkgs.haskellPackages.megaparsec_9_6_1
              ];
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
            tools = hp: {
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;

            hlsCheck.enable = false;
          };
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            settings = {
              treefmt.package = config.treefmt.build.wrapper;
            };
            hooks = {
              treefmt.enable = true;
            };
          };
        };

        just-flake.features = {
          treefmt.enable = true;
        };


        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = false;
          programs.hlint.enable = true;

          # Use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;

        };


        packages.default = self'.packages.elara;

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.just-flake.outputs.devShell
          ];
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          nativeBuildInputs = [ pkgs.just pkgs.convco ];

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
                        --no-nix \
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

