# See flake.nix (just-flake)
import 'just-flake.just'

default:
    @just --list

# Run the project tests with ghcid auto-recompile
test:
    stack build :elara-test --file-watch --fast --ghc-options="-O0 -fbyte-code"

# Run the project with ghcid auto-recompile
run:
   stack build --file-watch --fast --ghc-options='-O1 -fbyte-code' --exec "elara --dump-shunted --dump-core --dump-typed --run"

# Start Hoogle server for project dependencies
docs:
    echo http://127.0.0.1:8888
    stack hoogle -- serve -p 8888 --local