# wasm32-wasi build for running in a browser, optimised with wasm-opt
{ pkgs, elara }:
pkgs.runCommand "elara-wasm" { nativeBuildInputs = [ pkgs.binaryen ]; } ''
  mkdir -p $out/bin
  wasm-opt -Oz --enable-bulk-memory --enable-nontrapping-float-to-int --enable-sign-ext \
    --enable-mutable-globals --enable-tail-call \
    ${elara}/bin/elara.wasm -o $out/bin/elara.wasm
''
