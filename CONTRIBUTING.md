# Contributing to Elara

Elara is a small project and the compiler is usually being rewritten in some way, so it's always worth a quick chat before making any big-ish change \:)

## Where to talk

| For                                        | Use                                                                  |
| ------------------------------------------ | -------------------------------------------------------------------- |
| Questions, ideas, "would this be welcome?" | [GitHub Discussions](https://github.com/ElaraLang/elara/discussions) |
| Quick chat                                 | [Discord](https://discord.gg/xu5gSTV)                                |
| A bug, or a concrete task                  | [Issues](https://github.com/ElaraLang/elara/issues)                  |

**Please start a Discussion or Issue before a big change.** The internals frequently get reworked, so a large PR in those areas may conflict with work already in progress. Smaller changes can go straight to a PR.

## Getting set up

```sh
nix develop      # or install GHC and Cabal yourself
just test        # builds the JVM stdlib, then runs the test suite
just run examples/<file>.elr
```

Full instructions, including the non-Nix route, are in the [setup guide](https://elara.readthedocs.io/en/latest/compiler/internals/setup.html).

> `cabal test` won't compile the JVM stdlib, which can lead to test failures. `just test` handles this for you.

## Before you open a PR

- Run `just test` and `nix fmt`
- If the golden test output changes, and the change is intentional (e.g. you're updating a compiler error message), regenerate with `--golden-reset` (e.g. `just test --golden-reset`) and explain the diff in the PR description.
- Add a new test covering your change if relevant.
- Keep a PR to one change, where possible -- it makes it easier to review and keep track of!

## Commits

We use [Conventional Commits](https://www.conventionalcommits.org/) style. The [changelog](/CHANGELOG.md) is generated from these, so it's important that they're accurate.

**Scopes** match the compiler's architecture:

| Area                    | Scopes                                          |
| ----------------------- | ----------------------------------------------- |
| Front end               | `lexer`, `parser`, `desugar`, `rename`, `shunt` |
| Type system             | `type-infer`, `kind-infer`                      |
| Core                    | `core`, `to-core`                               |
| Back ends               | `interpreter`, `jvm`                            |
| Language                | `stdlib`, `prim`, `annotations`, `examples`     |
| Compiler infrastructure | `ast`, `query`, `errors`, `logging`, `cli`      |
| Project                 | `tests`, `docs`, `nix`, `ci`, `deps`            |

## Code style

- Keep comments minimal. Explain _why_, not _what_, and skip banner or section-header comments.
- Don't touch dependency bounds unless absolutely necessary.
- Try to match the existing code style where possible (though this isn't the most consistent anyway \:))

## Where to start

Look for [`good first issue`](https://github.com/ElaraLang/elara/labels/good%20first%20issue). The easiest places to make a real contribution without touching the compiler are:

- **The standard library** (`stdlib/*.elr`)
- **Examples** (`examples/`)
- **Documentation** (`docs/`)

## AI-generated contributions

Using AI tools is permitted, however:

- You must review and understand their output
- You must test any code yourself
- Significant AI usage **must** be disclosed
- Write in your own words in descriptions
- No bulk / slop PRs that just generically "fix issues"

## Licence

By contributing you agree that your contributions are licensed under the project's [MIT licence](LICENSE).
