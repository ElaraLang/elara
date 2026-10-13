{
  description = "Elara Programming Language";
  inputs = {
    hix = {
      url = "github:tek/hix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    git-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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

    weeder-nix = {
      url = "github:NorfairKing/weeder-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pre-commit-hooks.follows = "git-hooks-nix";
    };
  };

  nixConfig = {
    extra-substituters = [
      "https://elara.cachix.org"
      "https://cache.bristermitten.me/elara"
    ];
    extra-trusted-public-keys = [
      "elara.cachix.org-1:d129AMaRCJ6Q86oVUvIccZ1NsRZ6kyL5IkceevfQDWI="
      "elara:6GWLPsvcEXZFVCx4J8I8sB4jjppOB2/ISjt5oGXXkLo="
    ];
  };

  outputs =
    { self, flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [ ./nix/formatting.nix ];
      flake =
        let
          hixFlake = inputs.hix.lib.flake (import ./nix/hix.nix { inherit inputs self; });
          weederReport = import ./nix/weeder.nix { inherit (inputs) weeder-nix; };
        in
        # non-standard hix outputs fail flake-parts' schema check; gen-overrides fails cross-platform
        builtins.removeAttrs hixFlake [
          "debug"
          "lib"
          "overrides"
        ]
        // {
          apps = builtins.mapAttrs (_system: apps: builtins.removeAttrs apps [ "gen-overrides" ]) (
            hixFlake.apps or { }
          );

          # strip elara's lib output -- its .dyn_hi files drag GHC+docs into the closure
          packages = builtins.mapAttrs (
            system: pkgs:
            let
              # the dev env's nixpkgs, with hix's overlay (and so its package set) applied
              hixPkgs = hixFlake.legacyPackages.${system}.env.dev.pkgs;
            in
            pkgs
            // {
              weeder-report = weederReport hixPkgs;
            }
            // hixPkgs.lib.optionalAttrs hixPkgs.stdenv.hostPlatform.isLinux {
            }
          ) (hixFlake.packages or { });
        };
    };
}
