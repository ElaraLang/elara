{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
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
        inputs.mission-control.flakeModule
      ];

      perSystem = { self', lib, system, config, pkgs, ... }: {

        haskellProjects.default = {

          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell

          basePackages = pkgs.haskell.packages.ghc94;


          packages = {
            h2jvm.source = inputs.h2jvm;
            diagnose.source = inputs.diagnose;
            megaparsec.source = inputs.megaparsec;
          };

          settings = {

            fourmolu.check = false;


            diagnose = {
              extraBuildDepends = [
                pkgs.haskellPackages.megaparsec_9_4_1
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

        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              stack hoogle -- serve -p 8888 --local
            '';
            category = "Dev Tools";
          };

          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };

          run = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              stack build --file-watch --fast --ghc-options='-O0 -fbyte-code' --exec "elara --dump-core"
            '';
            category = "Primary";
          };
        };


        packages.default = self'.packages.elara;

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.mission-control.devShell
          ];
          inherit (self.checks.${system}.pre-commit-check) shellHook;

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
            ];
        };

      };
    };
}

