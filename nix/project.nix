{ pkgs }:
pkgs.haskell-nix.cabalProject' (
  { pkgs, ... }:
  {
    name = "elara";
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "elara-src";
      src = ../.;
    };
    compiler-nix-name = "ghc9124";
    modules = [
      (
        { pkgs, lib, ... }:
        lib.mkIf (!pkgs.stdenv.hostPlatform.isWasm) {
          # we run javac at build-time to bundle the JVM stdlib
          packages.elara.components.library.build-tools = [ pkgs.pkgsBuildBuild.jdk_headless ];
          # the JVM golden tests run the compiled classes with java
          packages.elara.components.tests.elara-test.build-tools = [ pkgs.pkgsBuildBuild.jdk_headless ];
        }
      )
      (
        { pkgs, lib, ... }:
        lib.mkIf pkgs.stdenv.hostPlatform.isWasm {
          # testing on wasm needs a teeny bit of setup:
          packages.elara.components.tests.elara-test.testWrapper = [
            "HOME=$(mktemp -d)" # make a random fake home directory to avoid any conflicts
            "${pkgs.pkgsBuildBuild.wasmtime}/bin/wasmtime"
            "--dir"
            "."
            "--dir"
            "$TMPDIR::/tmp" # default testWrapper has no /tmp but the stdlib eject tests need to write to /tmp
          ];
        }
      )
      (
        { pkgs, lib, ... }:
        lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
          # for static builds
          packages.elara.components.exes.elara.configureFlags =
            let
              ncurses = import ./ncurses.nix { inherit pkgs; }; # bundle our patched ncurses
            in
            [ "--extra-lib-dirs=${ncurses}/lib" ];

          # test that it's been statically linked correctly
          packages.elara.components.exes.elara.postInstall = ''
            ${lib.getExe pkgs.pkgsBuildBuild.file} $out/bin/elara | grep -q 'statically linked' \
              || { echo "elara is not statically linked" >&2; exit 1; }
          '';
        }
      )
    ];
    shell = {
      tools = {
        cabal = "latest";
        haskell-language-server = "latest";
      };
      buildInputs = with pkgs.pkgsBuildBuild; [
        haskellPackages.hpack
        jdk
        just
        just-lsp
        convco
        git-cliff
        nixfmt
        wasmtime
        mdbook
        mdbook-d2
        mdbook-variables
        d2
        nixd
        nodejs
        bun

        zlib
      ];
    };
  }
)
