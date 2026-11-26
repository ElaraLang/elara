# Sets up weeder to check for unused code
# It's a package rather than a check, because `nix flake check` failing on unused code would be quite annoying :)
{ pkgs, project }:
let
  # set up a separate build that also writes .hie files, so the normal build doesn't slow down and produce junk
  elara =
    (project.appendModule {
      modules = [
        {
          packages.elara.writeHieFiles = true;
          packages.elara.ghcOptions = [ "-hiedir=$hie" ];
        }
      ];
    }).hsPkgs.elara.components;
  weeder = project.tool "weeder" "latest";
in
pkgs.runCommand "weeder-report" { nativeBuildInputs = [ weeder ]; } ''
  weeder --config ${../weeder.toml} \
    --hie-directory ${elara.library.hie} \
    --hie-directory ${elara.exes.elara.hie} \
    --hie-directory ${elara.tests.elara-test.hie} \
    +RTS -G1 -N1 -RTS \
    > $out || [ $? -eq 228 ]
'' # the RTS flag is to workaround some GHC 9.12.4 runtime bug that makes weeder segfault
# weeder returns error code 228 if any weeds are found, which nix would otherwise
# interpret as something to mark the build as failed.
