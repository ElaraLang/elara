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
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {

      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];

      perSystem = { self', config, pkgs, ... }: {

        haskellProjects.default = {
          # basePackages = pkgs.haskell.packages.ghc94; # Use GHC 9.4.4

          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell

          overrides = self: super: with pkgs.haskell.lib; {
            megaparsec = self.callHackage "megaparsec" "9.3.0" { };

            diagnose = (dontCheck (self.callCabal2nix "diagnose"
              (
                pkgs.fetchgit {
                  url = "https://github.com/knightzmc/diagnose";
                  sha256 = "sha256-RqwnKus+Xga5nnV1g0lCQfqRg8zbN/IcDBZ+Sb1Bocs=";
                  rev = "7e8a1fe2bc3a60fdec43e96aab2a27fa1e3eb4d5";
                })
              { })).overrideAttrs
              (old: {
                buildInputs = old.buildInputs ++ [
                  self.megaparsec
                ];
                configureFlags = [ "-f megaparsec-compat" ]; # the flag configuration in stack.yaml seems to be ignoed
              });
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
          programs.hlint.enable = true;

          programs.ormolu.package = pkgs.haskellPackages.fourmolu;

        };

        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
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
              ghcid -c "stack ghci" --warnings -T :main --colour=always --restart='src,app,source.elr,prelude.elr'
            '';
            category = "Primary";
          };
        };


        packages.default = self'.packages.elara;
        apps.default = self'.apps.elara;

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

