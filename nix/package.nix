# elara's hix/cabal package
{
  buildInputs = pkgs: [
    pkgs.alex
  ];
  src = ../.;
  description = "See README for more info";

  override =
    { overrideAttrs, pkgs, ... }:
    drv:
    overrideAttrs (old: {
      unfilteredSrc = ../.;

      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.jdk ];
    }) drv;

  cabal = {
    author = "Alexander Wood";
    build-type = "Simple";
    copyright = "2022 Alexander Wood";
    license = "MIT";
    license-file = "LICENSE";
    version = "0.1.0";
    meta = {
      maintainer = "Alexander Wood <alexljwood24@hotmail.co.uk>";
      homepage = "https://github.com/ElaraLang/elara#readme";
      synopsis = "See README for more info";
      github = "ElaraLang/elara";

      extra-source-files = [
        "jvm-stdlib/**/*.java"
        "stdlib/**/*.elr" # embedded into the executable
      ];

      flags.static = {
        description = "Link the elara executable statically (set by the elara-static nix build)";
        manual = true;
        default = false;
      };
    };

    language = "GHC2024";
    prelude = {
      enable = true;
      package = "relude";
      module = "Prelude";
    };
    default-extensions = [
      "OverloadedStrings"
      "OverloadedRecordDot"
      "TypeFamilies"
      "LambdaCase"
      "ImportQualifiedPost"
      "DeriveDataTypeable"
      "DataKinds"
      "DeriveFunctor"
      "TypeApplications"
      "PartialTypeSignatures"
    ];
    dependencies = [
      "aeson"
      "algebraic-graphs"
      "array"
      "binary"
      "bytestring"
      "co-log-core"
      "co-log-effectful"
      "containers"
      "dependent-sum-template"
      "diagnose"
      "directory"
      "dependent-hashmap"
      "effectful >= 2.6.1.0"
      "effectful-core"
      "effectful-plugin"
      "effectful-th"
      "filepath"
      "generic-optics"
      "h2jvm"
      "hashable"
      "kind-generics-th"
      "lifted-base"
      "lsp"
      "lsp-types"
      "matrix"
      "megaparsec"
      "mtl"
      "optics"
      "parser-combinators"
      "pretty-simple"
      "prettyprinter"
      "prettyprinter-ansi-terminal"
      "process"
      "relude"
      "safe-exceptions"
      "some"
      "stringsearch"
      "template-haskell"
      "terminal-size"
      "text-metrics"
      "time"
      "utf8-string"
    ];
    ghc-options = [
      "-W"
      "-Wno-name-shadowing"
      "-Wno-partial-type-signatures"
      "-Widentities"
      "-optP-Wno-nonportable-include-path"
      "-fno-show-valid-hole-fits"
      "-fplugin=Effectful.Plugin"
      "-threaded"
      "-rtsopts"
    ];
  };
  library = {
    enable = true;
    source-dirs = "src";

    dependencies = [
      "witch"
      "th-abstraction"
    ];

    component = {
      build-tools = [
        "alex"
      ];
    };
  };
  executables.elara = {
    source-dirs = "app";
    dependencies = [
      "autodocodec"
      "opt-env-conf"
    ];
    component = {
      other-modules = [ "Paths_elara" ];
      verbatim = ''
        if flag(static)
          ld-options: -static
      '';
    };
  };
  tests.elara-test = {
    main = "Spec.hs";
    default-extensions = [
      "QuasiQuotes"
    ];
    dependencies = [
      "hedgehog"
      "sydtest"
      "sydtest-hedgehog"
      "template-haskell"
      "neat-interpolation"
    ];
    source-dirs = "test";
  };
}
