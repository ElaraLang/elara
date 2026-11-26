# Portable macOS builds.
# macOS doesn't support fully static executables, so instead we link the C libraries statically where macOS doesn't
# include them, and point the rest at the copies in /usr/lib. This removes any nix dependency at runtime.
{ project }:
(project.appendModule {
  modules = [
    (
      { pkgs, ... }:
      let
        ncurses = import ./ncurses.nix { inherit pkgs; };
        # gmp can be dynamically linked but we want it static
        gmp = pkgs.linkFarm "gmp-static" {
          "libgmp.a" = "${pkgs.gmp6.override { withStatic = true; }}/lib/libgmp.a";
        };
      in
      {
        packages.elara.components.exes.elara = {
          configureFlags = map (d: "--extra-lib-dirs=${d}") [
            gmp
            "${ncurses}/lib"
          ];
          # replace libiconv and libffi's nix paths with default macos ones
          postInstall = ''
            exe=$out/bin/elara
            # the first line of otool's output is the executable's own path
            for lib in $(otool -L $exe | awk 'NR > 1 && /\/nix\/store\// {print $1}'); do
              case $lib in
                */libiconv.2.dylib) system=libiconv.2.dylib ;;
                */libffi.*.dylib) system=libffi.dylib ;;
                *) echo "$exe links $lib, which macOS doesn't ship" >&2; exit 1 ;;
              esac
              install_name_tool -change $lib /usr/lib/$system $exe
            done
          '';
        };
      }
    )
  ];
}).hsPkgs.elara.components.exes.elara
