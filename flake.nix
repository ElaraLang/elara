{
  description = "Elara Programming Language";
  inputs = {
    haskellNix.url = "github:ican-group/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
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
  };

  nixConfig = {
    # haskell.nix computes the cabal plan during evaluation
    allow-import-from-derivation = true;
    extra-substituters = [
      "https://elara.cachix.org"
      "https://cache.bristermitten.me/elara"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "elara.cachix.org-1:d129AMaRCJ6Q86oVUvIccZ1NsRZ6kyL5IkceevfQDWI="
      "elara:6GWLPsvcEXZFVCx4J8I8sB4jjppOB2/ISjt5oGXXkLo="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };

  outputs =
    { flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      # the newest Hackage snapshot the locked haskell.nix can solve against, e.g. "2026-11-24T04:43:20"
      # `just bump-index-state` reads this to bump the `index-state` in `cabal.project
      flake.lib.latestIndexState = inputs.nixpkgs.lib.last (
        builtins.attrNames (import "${inputs.haskellNix.inputs.hackage}/index-state.nix")
      );
      imports = [ ./nix/formatting.nix ];
      perSystem =
        {
          config,
          system,
          lib,
          ...
        }:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            config = inputs.haskellNix.config // {
              # sydtest has a custom license which nixpkgs counts as unfree
              allowUnfreePredicate = pkg: lib.hasPrefix "sydtest" (lib.getName pkg);
            };
            overlays = [ inputs.haskellNix.overlay ];
          };
          project = import ./nix/project.nix { inherit pkgs; };
          flake = project.flake { crossPlatforms = p: [ p.wasi32 ]; };
          exes = p: p.hsPkgs.elara.components.exes;
          inherit (exes project) elara;
          # the musl cross target for this machine's architecture
          muslCross = if pkgs.stdenv.hostPlatform.isAarch64 then "aarch64-multiplatform-musl" else "musl64";
        in
        {
          _module.args.pkgs = pkgs;
          devShells = {
            default = flake.devShells.default.overrideAttrs (old: {
              # (re)install the pre-commit hook
              shellHook = (old.shellHook or "") + config.pre-commit.installationScript;
            });
            wasm = project.projectCross.wasi32.shellFor {
              # cabal builds build-tool-depends (i.e. alex) with the native ghc on PATH
              nativeBuildInputs = [ pkgs.haskell-nix.compiler.${project.args.compiler-nix-name} ];
            };
          };
          inherit (flake) checks;
          packages = {
            default = elara;
            inherit elara;
            weeder-report = import ./nix/weeder.nix { inherit pkgs project; };
            elara-wasm = import ./nix/wasm.nix {
              inherit pkgs;
              inherit (exes project.projectCross.wasi32) elara;
            };
          }
          // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
            elara-static = (exes project.projectCross.${muslCross}).elara;
          }
          // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
            elara-portable = import ./nix/portable-darwin.nix { inherit project; };
          };
        };
    };
}
