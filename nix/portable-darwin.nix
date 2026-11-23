# Portable macOS builds.
# macOS doesn't support fully static executables, so instead we link the C libraries statically where macOS doesn't
# include them, and point the rest at the copies in /usr/lib. This removes any nix dependency at runtime.
{
  hixPkgs,
}:
let
  inherit (hixPkgs) lib;
  inherit (hixPkgs.haskell.lib) overrideCabal justStaticExecutables;
  staticArchives = [
    "${hixPkgs.gmp6.override { withStatic = true; }}/lib/libgmp.a"
    "${hixPkgs.ncurses.override { enableStatic = true; }}/lib/libncursesw.a"
  ];
in
overrideCabal (justStaticExecutables hixPkgs.__hix.packages.elara) (drv: {
  doCheck = false;
  enableSharedLibraries = false;
  # GHC still passes -lgmp/-lncursesw, which ld64 resolves to the dylibs; force-loading the archives defines every
  # symbol up front, so those dylibs end up unused and get dropped
  configureFlags =
    (drv.configureFlags or [ ])
    ++ map (opt: "--ghc-option=-optl-Wl,${opt}") (
      map (a: "-force_load,${a}") staticArchives ++ [ "-dead_strip_dylibs" ]
    );
  # nix's libiconv and libffi are Apple's own, so the system copies are drop-in replacements
  postInstall = (drv.postInstall or "") + ''
    for b in $out/bin/*; do
      for lib in $(otool -L "$b" | awk 'NR > 1 && /\/nix\/store\// {print $1}'); do
        case "$(basename "$lib")" in
          libiconv.2.dylib) install_name_tool -change "$lib" /usr/lib/libiconv.2.dylib "$b" ;;
          libffi.*.dylib) install_name_tool -change "$lib" /usr/lib/libffi.dylib "$b" ;;
          *) otool -L "$b" >&2; echo "$b links $lib, which macOS doesn't ship" >&2; exit 1 ;;
        esac
      done
    done
  '';
})
