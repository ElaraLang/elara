# Changelog

### [v0.0.2](https://github.com/ElaraLang/elara/compare/v0.0.1...v0.0.2) (2024-06-01)

#### Features

* **renamer:** Add a nice pretty error report for when the renamer catches
ambiguous names
([f31a6cd](https://github.com/ElaraLang/elara/commit/f31a6cd94be1e86fcd419fcc2d0a47b79e08e086))

#### Fixes

* **renamer:** Fix heavy-handed ambiguous name checking reporting ambiguity
when there actually isn't any! Fixes variable shadowing
([ecb3481](https://github.com/ElaraLang/elara/commit/ecb3481cd8580e8b1ac9cfca42413f0996e5ad81))

#### Other

* **renamer:** Change renaming backend to allow multiple names with the same
unqualified name
([f61f155](https://github.com/ElaraLang/elara/commit/f61f155c4f2c57e069ff82fa1cf7149d69d0dbdf))

#### Documentation

* **changelog:** Update CHANGELOG.md
([0882b33](https://github.com/ElaraLang/elara/commit/0882b339938554cd083a197b1a4b7092f23b905c))

### [v0.0.1](https://github.com/ElaraLang/elara/compare/v0.0.0...v0.0.1) (2024-06-01)

#### Features

* **tocore:** Improve reporting of `UnknownConstructor` error in ToCore
([8fda8b3](https://github.com/ElaraLang/elara/commit/8fda8b31a101f0e70c70e5cdd21ae9967f475a66))
* **stdlib:** Add Option module to stdlib
([646f1ae](https://github.com/ElaraLang/elara/commit/646f1aefa5b2e6d9ef88c4401acfac24b91369e7))
* **stdlib:** Load all the stdlib files dynamically based on what's in the new
stdlib folder
([47e0e90](https://github.com/ElaraLang/elara/commit/47e0e90b71f41d830fe269f7cb99224ef7c0b1b9))

#### Fixes

* **tocore:** Fix ToCore not being invoked in topological order, causing
unknown name errors
([8737b28](https://github.com/ElaraLang/elara/commit/8737b281c63b8b59055d0c3d1d319f8c31a41119))

#### Other

* **actions:** that seemed to work - now remove the logging
([31478e6](https://github.com/ElaraLang/elara/commit/31478e679894b86feb899a8fa50e3b18babde244))
* **actions:** Log big files
([282fb34](https://github.com/ElaraLang/elara/commit/282fb34aec3685f45f1a086d3a4fc418a1412624))
* **actions:** nevermind let's not do that
([a30d954](https://github.com/ElaraLang/elara/commit/a30d954a686e116764c127e77b0b9d228af40a3c))
* **actions:** Run linux builds on self-hosted runner
([9f8f291](https://github.com/ElaraLang/elara/commit/9f8f291e6e3dab4d50b3682a02e9b7a0c2c6fb42))
* **actions:** Remove blank line
([20b7d8a](https://github.com/ElaraLang/elara/commit/20b7d8a958a29db2a324fc8987980873f8270807))
* **actions:** Change the script...?
([4ab2c8f](https://github.com/ElaraLang/elara/commit/4ab2c8fa44b3aa0f910bced1b8e663c853594d99)),
closes [#19](https://github.com/ElaraLang/elara/issues/19)
* **actions:** Try deleting this folder
([c35171c](https://github.com/ElaraLang/elara/commit/c35171c71d049f4b468ceedea39f97c8407cbcaf))
* **stdlib:** Make main file a bit more fully-fledged
([32b919c](https://github.com/ElaraLang/elara/commit/32b919c8358bb844b74228c2148dac3cd53a94d0))
* **stdlib:** Move all stdlib files into new folder
([e7cd134](https://github.com/ElaraLang/elara/commit/e7cd1344aa8c04dafc79c2ff8f537985cc62708c))

## v0.0.0 (2024-06-01)

### ⚠ BREAKING CHANGE

* stdlib signature changed


### Features

* **type-infer:** Add a new error message in the case of multiple tyapps being
detected (which shouldn't happen?)
([7486ea1](https://github.com/ElaraLang/elara/commit/7486ea12c758a85334ff48556be6365bc63302fa))
* **logging:** Determine the log level based on elaraDebug variable
([93f8f8a](https://github.com/ElaraLang/elara/commit/93f8f8aefbe115ccd7bedaac3a404d15e3ececce))
* **pretty-printing:** Remove the domain when printing foralls (as it will
only ever be type)
([2b5158e](https://github.com/ElaraLang/elara/commit/2b5158e3eb9459fb67079669025794a7fa81910a))
* **pretty-printing:** Improve colourisation of pretty-printer output
([28492c5](https://github.com/ElaraLang/elara/commit/28492c57370c98ea8142574261dc36d1b80185b9))
* **stdlib:** Add primitive functions for working with strings
([a582f06](https://github.com/ElaraLang/elara/commit/a582f0619b44bd20fc8fbbcfef4c744eb06a58fb))
* **stdlib:** Restore the stdlib to its former glo
([0fc83e9](https://github.com/ElaraLang/elara/commit/0fc83e918b2ecf1422cf08c226d7f49442f175b7))

### Fixes

* **type-infer:** Improve algorithm for detecting tyapps, fixing some edge
cases
([8503c23](https://github.com/ElaraLang/elara/commit/8503c23109a8f79180a70b77393040141d6527df))
* **codegen:** Fix stringLength primitive not boxing integers
([85fcf5a](https://github.com/ElaraLang/elara/commit/85fcf5a6e72d2cd27f68c0854de3bf54e7b54b80))
* **logging:** Remove old debugPretty calls
([64edc03](https://github.com/ElaraLang/elara/commit/64edc0310f539740bc81138b6b042b9333501617))
* **type-infer:** Fix if/else type inference not always unifying correctly and
throwing errors
([33a72c2](https://github.com/ElaraLang/elara/commit/33a72c2103a1657dc9626176c2eb683513e9d4bf))
* **tests:** Fix unit tests not compiling for various reasons
([e0a2955](https://github.com/ElaraLang/elara/commit/e0a2955d618e7a6ea1d244d2cf4672fb476c51ed))
* **type-infer:** Fix type inference logging _always_ being on debug
([3b34bb1](https://github.com/ElaraLang/elara/commit/3b34bb1e216fafad98cf08c1d0dec4f242b61587))
* **type-infer:** Fix constructor patterns not being completely solved
([1dc7624](https://github.com/ElaraLang/elara/commit/1dc7624f1dbaeab084a4fb7e3ae23170eb92337a))
* **type-infer:** Fix conversion between types when explicitly applying type
constructors (eg `def x : Option Int`)
([940e960](https://github.com/ElaraLang/elara/commit/940e9608a699d06b9324f169cc1367afc43907d9))
* Fix code generation for constructor patterns with multiple arguments
breaking
([6de3e20](https://github.com/ElaraLang/elara/commit/6de3e203a2d6ac90b98e7aea835062189a717621))
* **type-infer:** Fix constructor pattern arguments being reversed in type
inference
([321fdd6](https://github.com/ElaraLang/elara/commit/321fdd6fbe6aaad25a547006d753a97f616e18d9))

### Other

* **actions:** that seemed to work - now remove the logging
([f9a9294](https://github.com/ElaraLang/elara/commit/f9a9294cc28589d3204c5c1e3c4e84f0a95764cf))
* **actions:** Log big files
([e0158fd](https://github.com/ElaraLang/elara/commit/e0158fd46eb94fcaaf9df45b06dd4ceb8526e4fc))
* **actions:** nevermind let's not do that
([c684cb9](https://github.com/ElaraLang/elara/commit/c684cb95ab3c53c4a2508ccc8a8b676b5c6f7cc7))
* **actions:** Run linux builds on self-hosted runner
([e47fa1a](https://github.com/ElaraLang/elara/commit/e47fa1acbdc2e180788bd8c008331baaa665392a))
* **actions:** Remove blank line
([9b61673](https://github.com/ElaraLang/elara/commit/9b6167309e788bcaa4adc40493db610ef88a70c3))
* **actions:** Change the script...?
([27dc8ea](https://github.com/ElaraLang/elara/commit/27dc8eaac5a23a955ae7336e96320b95663906d2)),
closes [#19](https://github.com/ElaraLang/elara/issues/19)
* **actions:** Try deleting this folder
([fb8c8bd](https://github.com/ElaraLang/elara/commit/fb8c8bd56ee9b231b39e1004123883934913c0a6))
* **stdlib:** Change `readFile` to return String rather than EList<String>
([01f4048](https://github.com/ElaraLang/elara/commit/01f40486c687c47d8f17a7795931d65c4f606e11))
* **type-infer:** Uncomment test for `forall a. a` tyapps
([e2786a0](https://github.com/ElaraLang/elara/commit/e2786a056b7ab8ba46b96616cc953f6da3e84736))
* **stdlib:** Remove random list from main file
([85524a1](https://github.com/ElaraLang/elara/commit/85524a108d84963b0b7090093ffb952e9d4ff4f8))
* **type-infer:** Add new tests for type application insertion
([a341c65](https://github.com/ElaraLang/elara/commit/a341c6597677d6ff814d73cd6896231e7ed3f8b4))
* **nix:** Update nix flake
([f95de02](https://github.com/ElaraLang/elara/commit/f95de028cc2349f71901f8c5c7e7b0cd8e4b1558))
* **stdlib:** Refactor Elara Stdlib and add list length function
([2fa03be](https://github.com/ElaraLang/elara/commit/2fa03be4f9acc294bc2d0a41f5314fb9aceba625))
* **stdlib:** Move ALL mentions of lists to the stdlib - they are no longer a
primitive!
([8e3d367](https://github.com/ElaraLang/elara/commit/8e3d367d94078552ea642851654c7abe7b7ff42e))
* **logging:** Change back to info logging (really need to make this
configurable at runtime)
([7666ae9](https://github.com/ElaraLang/elara/commit/7666ae96126238a5f453a4462aedb692d0316093))
* **tocore:** Change weird usage of =<< in ToCore
([936ab2a](https://github.com/ElaraLang/elara/commit/936ab2a3ec0b740fabae2c0954cc29d1f4cb4b24))
* Begin work refactoring lambda generation - keep track of parameter names
_eerywhere_
([9f8ed70](https://github.com/ElaraLang/elara/commit/9f8ed708d95cab1b5106606d15e96aa9d3aecd91))
* Slightly tidy up generation of lambdas
([4654676](https://github.com/ElaraLang/elara/commit/4654676b73c2fa3258d9b3bf044f56e7a2f6e677))
* **nix:** Add the old mission-control scripts back to the justfile
([1508631](https://github.com/ElaraLang/elara/commit/15086319ecb703d2774a436775312f7409df0489))
* **nix:** Move from mission-control to just
([ba41767](https://github.com/ElaraLang/elara/commit/ba4176752274ae6b6cdbc870d8bc1d808378380d))

### Documentation

* **stdlib:** Remove mention of EList from readme compiling guide
([624a156](https://github.com/ElaraLang/elara/commit/624a156569ef71567788a0d380ea13212dd5d9c5)),
closes [#12](https://github.com/ElaraLang/elara/issues/12)
