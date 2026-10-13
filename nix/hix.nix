# hix project config (https://github.com/tek/hix): compiler, environments, dependency overrides and the dev shell
{ inputs, self }:
{ config, ... }:
let
  sharedOverrides =
    {
      source,
      enable,
      notest,
      unbreak,
      jailbreak,
      nodoc,
      ...
    }:
    {
      h2jvm = nodoc (notest (source.root inputs.h2jvm));
      diagnose = nodoc (enable "megaparsec-compat" (source.root inputs.diagnose));
      dependent-hashmap = nodoc (source.root inputs.dependent-hashmap);
      co-log-effectful = nodoc (jailbreak unbreak);
      optics = nodoc notest; # test fails on ghc 9.12
      generic-optics = nodoc notest; # inspection tests fail on ghc 9.12
    };
in
{
  compiler = "ghc912";
  build-tools.hpack.package = config.pkgs.hpack;
  build-tools.cabal.package = config.pkgs.cabal-install;
  systems = import inputs.systems;
  compat.enable = false;
  envs.latest.ghcid.enable = false;
  envs.latest.hls.enable = true;
  # we don't need this and it slows down CI for very little benefit
  envs.dev.libraryProfiling = false;
  envs.latest.libraryProfiling = false;
  envs.lower.libraryProfiling = false;

  managed = {
    enable = true;
    latest.compiler = "ghc912";
    lower.enable = true;
    lower.compiler = "ghc912";

    envs = {
      solverOverrides = sharedOverrides;
      verbatim.overrides = sharedOverrides;
    };
  };
  outputs.devShells = {
    # extending the default devshell to add the pre-commit hooks and some other nice things
    default = config.pkgs.mkShell {
      inputsFrom = [
        config.outputs.devShells.latest # the managed latest env doubles as the devshell
        self.devShells.${config.system}.pre-commit # the pre-commit devshell
      ];
      buildInputs = with config.pkgs; [
        just
        just-lsp
        convco
        git-cliff
        nixfmt

        mdbook
        mdbook-d2
        mdbook-variables
        d2
        nixd
      ];
    };
  };

  overrides = sharedOverrides;
  packages.elara = import ./package.nix;
}
