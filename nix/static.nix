# Fully static builds.
# This is a completely horrific mess and has to build multiple GHCs from source. Relying on the binary cache is basically the only practical option
# Also mostly AI generated because I am not going to spend 20 hours trial-and-erroring this.
# AI Comments left unadulterated for "clarity"
{
  # the dev env's nixpkgs, with hix's overlay (and so its package set) applied
  hixPkgs,
}:
let
  inherit (hixPkgs) lib;
  muslPkgs = hixPkgs.pkgsMusl.appendOverlays [
    (_final: prev: {
      # build-time tools only need to run here, so take the cached glibc builds: a musl cabal2nix (hix's IFD) would
      # build nix+llvm from source, and musl alex would need a source-built GHC 9.10
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = lib.composeExtensions (old.overrides or (_: _: { })) (
          _: _: { inherit (hixPkgs.haskellPackages) cabal2nix-unstable alex; }
        );
      });
      haskell = prev.haskell // {
        # every source-built GHC here (ours and its 9.10 boot compiler): run hadrian with the cached glibc python,
        # skip the docs (they'd need a musl sphinx/python set), and drop DWARF support, whose elfutils would otherwise
        # need static linking into elara and a musl python to build
        compiler = lib.mapAttrs (
          _: ghc:
          if lib.isDerivation ghc && lib.functionArgs ghc.override ? python3 then
            ghc.override {
              python3 = hixPkgs.python3;
              enableDocs = false;
              enableDwarf = false;
            }
          else
            ghc
        ) prev.haskell.compiler;
      };
    })
  ];
  withStaticLibs =
    pkg:
    pkg.overrideAttrs {
      dontDisableStatic = true;
    };
in
muslPkgs.haskell.lib.overrideCabal
  (muslPkgs.haskell.lib.justStaticExecutables muslPkgs.__hix.packages.elara)
  (drv: {
    doCheck = false;
    # linking elara's own .so against the static C libs below fails
    enableSharedLibraries = false;
    configureFlags =
      (drv.configureFlags or [ ])
      ++ [ "-fstatic" ]
      # nixpkgs disables static C libs by default
      ++ map (p: "--extra-lib-dirs=${p}/lib") [
        (muslPkgs.gmp6.override { withStatic = true; })
        (withStaticLibs muslPkgs.libffi)
        (muslPkgs.ncurses.override { enableStatic = true; })
        (withStaticLibs muslPkgs.numactl)
      ];
    postInstall = (drv.postInstall or "") + ''
      for b in $out/bin/*; do
        ${lib.getExe hixPkgs.file} "$b" | grep -qE 'statically linked|static-pie linked' \
          || { echo "$b is not statically linked" >&2; exit 1; }
      done
    '';
  })
