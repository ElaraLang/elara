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

    h2jvm.url = "github:ElaraLang/h2jvm";
    # h2jvm.flake = fa

    diagnose.url = "github:knightzmc/diagnose";
    diagnose.flake = false;

  };

  outputs = inputs@{ self, nixpkgs, ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {

      systems = import inputs.systems;

      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule

      ];

      perSystem = { self', system, config, pkgs, ... }: {
        _module.args.pkgs = import self.inputs.nixpkgs {
          inherit system;
          overlays =
            let

              ghcName = "ghc92";
              # Desired new setting
              enableProfiling = false;
            in

            [
              (final: prev:
                let
                  inherit (final) lib;
                in

                {
                  haskell = lib.recursiveUpdate prev.haskell {
                    compiler.${ghcName} = prev.haskell.compiler.${ghcName}.override {
                      # Unfortunately, the GHC setting is named differently for historical reasons
                      enableProfiledLibs = enableProfiling;
                    };
                  };
                })

              (final: prev:
                let
                  inherit (final) lib;
                  haskellLib = final.haskell.lib.compose;
                in

                {
                  haskell = lib.recursiveUpdate prev.haskell {
                    packages.${ghcName} = prev.haskell.packages.${ghcName}.override {
                      overrides = hfinal: hprev: {
                        mkDerivation = args: hprev.mkDerivation (args // {
                          enableLibraryProfiling = enableProfiling;
                          enableExecutableProfiling = enableProfiling;
                        });
                      };
                    };
                  };
                })
            ];
        };

        haskellProjects.default = {

          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell

          basePackages = pkgs.haskell.packages.ghc92;



          packages = {
            h2jvm.source = inputs.h2jvm;
            diagnose.source = inputs.diagnose;
          };

          settings = {

            diagnose = {
              extraBuildDepends = [ pkgs.haskellPackages.megaparsec ];
              extraConfigureFlags = [ "-f megaparsec-compat" ];
            };

            h2jvm = {
              # Skip the tests for now
              check = false;
            };

          };

          devShell = {
            tools = hp: {
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;

            hlsCheck.enable = false;
          };
        };



        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = false;
          programs.hlint.enable = false;

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
              ghcid -c "stack repl --ghc-options='-g -ferror-spans -fdiagnostics-color=always'" --warnings -T ':main --dump-typed --dump-core' --colour=always --restart=source.elr
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
            ];
        };

      };
    };
}

