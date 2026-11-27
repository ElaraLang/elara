# sets up nix-managed node_modules for editors/'s npm packages, and a flake check that typechecks them
{ pkgs }:
let
  inherit (pkgs) lib;

  mkNodeModules =
    npmRoot:
    pkgs.importNpmLock.buildNodeModules {
      inherit npmRoot;
      inherit (pkgs) nodejs;
    };
in
rec {
  nodeModules = {
    textmate = mkNodeModules ../editors/textmate;
    vscode = mkNodeModules ../editors/vscode;
  };

  typecheck = pkgs.runCommand "elara-editors-typecheck" { } ''
    cp -r ${../editors} editors
    chmod -R +w editors
    ${lib.concatMapAttrsStringSep "\n" (name: modules: ''
      ln -s ${modules}/node_modules editors/${name}/node_modules
      (cd editors/${name} && node_modules/.bin/tsc)
    '') nodeModules}
    touch $out
  '';
}
