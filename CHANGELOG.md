# Changelog

All notable changes to this project will be documented in this file.

Note that while Elara follows SemVer where possible, most of the source code is not considered public API with regards to breaking changes.
As such, breaking changes may occur between minor versions until a stable release of the language.

## [unreleased]

### 🚀 Features

- *(ci)* Update Nix CI Action to run flake checks - ([112168d](https://github.com/ElaraLang/elara/commit/112168de4b97944bc661ff90c802f16d47854f6a)) 
- *(core)* :art: Add the primitive `doubleCon` name to Elara.Prim.Core - ([78cf21b](https://github.com/ElaraLang/elara/commit/78cf21bba1c6e27123db80c5c0d62373342ad8fd)) 
- *(core)* :sparkles: Add a function to calculate ("infer"?) the type of a core expression - ([34ac8e6](https://github.com/ElaraLang/elara/commit/34ac8e633e92cb8343b92f6389f7680c870d4eb0)) 
- *(core)* :sparkles: Core type checking! - ([72cac00](https://github.com/ElaraLang/elara/commit/72cac00c63ae90e7baedb6342261b1e631d2ffd9)) 
- *(core)* :sparkles: Add match inference for core tc - ([17af4fc](https://github.com/ElaraLang/elara/commit/17af4fc8d9e41338955bdcf0404283904214d367)) 
- *(core)* :bug: Fix/improve core type checking of patterns - ([fe74822](https://github.com/ElaraLang/elara/commit/fe74822ecd5e6dcbe1c8b7b4fdbc15bd2dc9ab72)) 
- *(core)* :safety_vest: Add a check for pattern matches missing binders in core typecheck - ([9f6eac6](https://github.com/ElaraLang/elara/commit/9f6eac69f6622529cff0325f3c79a27c3f346d6c)) 
- *(interpreter)* :sparkles: It works! Mostly - ([49d49a2](https://github.com/ElaraLang/elara/commit/49d49a23d762961e8b028d5223d021eb0949201f)) 
- *(interpreter)* :sparkles: Interpreter correctly handles type applications now! - ([6e23029](https://github.com/ElaraLang/elara/commit/6e230296cae98d4d05cc7e290c860552a2ebb1ff)) 
- *(interpreter)* Add more prim ops - ([f165b89](https://github.com/ElaraLang/elara/commit/f165b89235abf4cc511c20fa58fbef2ae6f5ea35)) 
- *(interpreter)* Make stringCons primitive work - ([6e0db02](https://github.com/ElaraLang/elara/commit/6e0db02b4b26a7f481f39e93b50797f5bdb5cb19)) 
- *(interpreter)* Add a hardcoded toString case for Tuples - ([5d83ff2](https://github.com/ElaraLang/elara/commit/5d83ff27912f33204cff57c399ce676d5bd5750e)) 
- *(logging)* Write logs to a file because they're getting big! And properly use terminal width now - ([3fe538c](https://github.com/ElaraLang/elara/commit/3fe538c928aa795d65e2e006bce390382df927c6)) 
- *(logging)* Improve pretty output of some elements - ([7e17b22](https://github.com/ElaraLang/elara/commit/7e17b228cca625043d7671e8c88d1f41585fd8cc)) 
- *(nix)* Enable actionlint in treefmt configuration - ([ce014f6](https://github.com/ElaraLang/elara/commit/ce014f63a4beb4f0a45abe819fb94f33f18944a8)) 
- *(pattern-matching)* Make pattern matching compilation work... - ([b15cc93](https://github.com/ElaraLang/elara/commit/b15cc937f2c580ede3ad40c8f2f4ddcb4e4e6b86)) 
- *(pretty)* Add Pretty CallStack instance - ([5644720](https://github.com/ElaraLang/elara/commit/56447201839ba9c9c340e4f23d8fa56d687bc595)) 
- *(pretty)* Highlight `def` as a keyword - ([d9e552f](https://github.com/ElaraLang/elara/commit/d9e552f25b0f0176a2abbdfe810845feba56d489)) 
- *(stdlib)* General stdlib improvements - ([5acb092](https://github.com/ElaraLang/elara/commit/5acb092f56a8a3a9d6fc401497109babe1f9c26b)) 
- *(stdlib)* Support (x, y) tuple syntax in patterns - ([6f157ff](https://github.com/ElaraLang/elara/commit/6f157ff194c7650ded1ce1c9e6f94b22d834aa1e)) 
- *(to-core)* :zap: Add a core-to-core pass which removes reundant let-in's - ([0d3bc53](https://github.com/ElaraLang/elara/commit/0d3bc5397c8b6754c3ebd2c64373844249d78684)) 
- *(to-core)* :sparkles: Get very basic (and probably super buggy) conversion to ANF working! - ([2283e6c](https://github.com/ElaraLang/elara/commit/2283e6c089058a6d2a80617a3f4c177b62e772e4)) 
- *(to-core)* :sparkles: Start to reimplement to-core phase - ([7e46451](https://github.com/ElaraLang/elara/commit/7e46451412cc79c9dba40281d85b8e721923038b)) 
- *(to-core)* :sparkles: Add the main conversion functionality back - ([35201bd](https://github.com/ElaraLang/elara/commit/35201bda746ed2aef70e7cda3b513fc053f49801)) 
- *(to-core)* :sparkles: Implement recursive let detection for to-core - ([8c72069](https://github.com/ElaraLang/elara/commit/8c72069ff3aba2fb72206de4ce9ee9b5202f7574)) 
- *(to-core)* Add ADT to-core back - ([f831ee5](https://github.com/ElaraLang/elara/commit/f831ee523d97fce5069ee8ebc7c67768c5df3f3b)) 
- *(type-infer)* :lipstick: Improve output of NotFunctionType error - ([2508123](https://github.com/ElaraLang/elara/commit/25081239a78878b089460c85e2c0465072b0ccb2)) 
- *(type-infer)* Basic type inference for ADTs - ([c259e89](https://github.com/ElaraLang/elara/commit/c259e89c348f33b0067619e2cbe8d3ce963250d3)) 
- *(type-infer)* :sparkles: Add type applications! - ([542189b](https://github.com/ElaraLang/elara/commit/542189b1e391d2c9c2cf840dac0678a43a9483ce)) 
- :sparkles: Add pretty instance for 6-tuple - ([1189208](https://github.com/ElaraLang/elara/commit/118920892de6df9abac0afc92ff733203f01765f)) 
- :sparkles: Allow certain AST elements to be assigned polytypes - ([43022fe](https://github.com/ElaraLang/elara/commit/43022fe145869564df71a763160b870992a1b762)) 
- Add more error codes - ([0792ed4](https://github.com/ElaraLang/elara/commit/0792ed499722d5e5628a0cb689a74576ff6d7f1a)) 

### 🐛 Bug Fixes

- *(cabal)* Make sure we compile with -threaded - ([4d9d4c5](https://github.com/ElaraLang/elara/commit/4d9d4c54a006e4a95923056ceb6cda847c4dfa85)) 
- *(core)* :bug: Fix lambda ANF transformations - ([a3c86e9](https://github.com/ElaraLang/elara/commit/a3c86e98eebadfc0a4416876e673e212ab036d61)) 
- *(core)* :bug: Fix recursive lets (aptly) infinite looping when being converted to ANF - ([34a0633](https://github.com/ElaraLang/elara/commit/34a063303e27f825a41787172ee4366ef8c75a4e)) 
- *(core)* :bug: Fix ToANF incorrectly handling lets - ([6dec072](https://github.com/ElaraLang/elara/commit/6dec0726cf8052869ff5f400d38c192bc18e2019)) 
- *(core)* :bug: Fix core typecheck incorrectly reporting unbound vars - ([b5e0972](https://github.com/ElaraLang/elara/commit/b5e0972800eaee8c5f40c800bd9ce5c2257e2fa0)) 
- *(core)* :bug: Fix/implement 'HasDependencies (CoreModule bind)' - ([9230cd1](https://github.com/ElaraLang/elara/commit/9230cd101020bb1bad4476a7422982bcc9320907)) 
- *(core)* :bug: Fix `exprType` for polymorphic function applications - ([29fe7cc](https://github.com/ElaraLang/elara/commit/29fe7cc0fe087cee15bbee9efba86a26e83a9372)) 
- *(core)* :bug: Fix core type checking for some cases - ([c0c0c01](https://github.com/ElaraLang/elara/commit/c0c0c01cfe67851c276052157fc53cbcb22ee26f)) 
- *(core)* :bug: Fix recursive let binds not being converted to ANF correctly - ([d6ffdee](https://github.com/ElaraLang/elara/commit/d6ffdee9bcff8039d2d8e969ed1f17007c6f298b)) 
- *(core)* Correct implementation of equalUnderSubst function and tidy up code - ([b96b7fc](https://github.com/ElaraLang/elara/commit/b96b7fc329e405a1b47a0d80713f91dec276c2ab)) 
- *(flake)* :bug: Update flake.lock and flake.nix for dependency resolution (NO MORE DEPENDENCY HELL) - ([3762c6d](https://github.com/ElaraLang/elara/commit/3762c6d0e8f830b450699353ce27f9c975b2ff63)) 
- *(flake)* Tidy up nix and fix various dependency bugs - ([65bd7d4](https://github.com/ElaraLang/elara/commit/65bd7d4e8c130dd631df5aca4a3c6294855fb20b)) 
- *(lexer)* Make lexer much more robust and flexible about the indentation levels it accepts - ([ac594d3](https://github.com/ElaraLang/elara/commit/ac594d3557ccede823221de511ba80b2372913b7)) 
- *(logging)* :bug: Fix call stacks in logging - ([d3e4620](https://github.com/ElaraLang/elara/commit/d3e4620b9bfc069972393c8a31e3b773002b7d04)) 
- *(main)* Fix file locked error when dumping certain information - ([bd87a13](https://github.com/ElaraLang/elara/commit/bd87a132b083b23cca49066b8b57314dc91d279e)) 
- *(parser)* :bug: Fix type declaration parsing wrt indents - ([53f148b](https://github.com/ElaraLang/elara/commit/53f148beae28c2ef2465cc2fdd080206f556b2d9)) 
- *(parser)* :bug: Fix operators as variables (eg `(+)`) not being correctly parsed - ([55de02d](https://github.com/ElaraLang/elara/commit/55de02dcaeb34707588f2824b12a1f25bc6e382c)) 
- *(parser)* :bug: Fix Nested Constructor Patterns not being correctly parsed - ([bfae8ac](https://github.com/ElaraLang/elara/commit/bfae8acea7d3225329b40f1e588361d2f01a1451)) 
- *(parser)* :bug: Fix parser incorrectly handling some block inputs - ([fcf89e7](https://github.com/ElaraLang/elara/commit/fcf89e730fef162751034ec3647334a58c9d2bb2)) 
- *(prim)* Add primitive kind for List - ([3773ad0](https://github.com/ElaraLang/elara/commit/3773ad0ff3bad9e0506a1cee98d27b3e4916de89)) 
- *(prim)* Add missing primitive type `Bool` to primitiveTypes - ([dd369a3](https://github.com/ElaraLang/elara/commit/dd369a35f30a07ef4f54f5f080a460d7177d7091)) 
- *(rename)* Fix renaming of recursive lets in a block - ([264db37](https://github.com/ElaraLang/elara/commit/264db37d935fef117d5183b28280b48613631faa)) 
- *(renamer)* Now qualifies type names - ([0c25cad](https://github.com/ElaraLang/elara/commit/0c25cade010b873a4a3fead0cc3df7dad5b64f3e)) 
- *(shunter)* Fix shunter query not being wired properly - ([f0ba43a](https://github.com/ElaraLang/elara/commit/f0ba43a7dc78d71fbe2b5dab40930aa446d70fdb)) 
- *(stdlib)* :pencil2: Fix a "typo" in source.elr (so that it works) - ([9e1f1fd](https://github.com/ElaraLang/elara/commit/9e1f1fd78c435d78117de605a05f3442c654b823)) 
- *(tests)* :test_tube: Fix the unit tests not compiling / succeeding - ([730e828](https://github.com/ElaraLang/elara/commit/730e828cd6bfd09e3d05cc5361b0030a55a7b364)) 
- *(tests)* :bug: Fix tests failing due to logging not working in parallel - ([33032d6](https://github.com/ElaraLang/elara/commit/33032d67c6c1364b717a8717d8716fc4952f922c)) 
- *(to-core)* :bug: Fix ANF conversion in core not correctly handling match expressions - ([6cc25cd](https://github.com/ElaraLang/elara/commit/6cc25cdafde330f383ecbdbcc9b0e639ba3fcc28)) 
- *(to-core)* :bug: Fix global recursive bindings not being created as recursive - ([ad3216f](https://github.com/ElaraLang/elara/commit/ad3216f2b2a9f6dd1dd44967d44c34ecb635d1bb)) 
- *(to-core)* :label: Fix ANF Core type mismatch errors - ([7e39c4e](https://github.com/ElaraLang/elara/commit/7e39c4e44b57d58e63876070411bd211915a8746)) 
- *(to-core)* :bug: Fix order of type variables/foralls when converting data constructors to core - ([c9982f0](https://github.com/ElaraLang/elara/commit/c9982f06007679620dd3a70b359c8346858d5d01)) 
- *(to-core)* :bug: Fix recursive lets not being converted to ANF properly - ([3ffd19b](https://github.com/ElaraLang/elara/commit/3ffd19b13719acff03d505d907d38bb3d49dd431)) 
- *(to-core)* Fix core match desugaring not correctly typing patterns - ([f5726c2](https://github.com/ElaraLang/elara/commit/f5726c2f0be59a241d345151b63e0bfdc3d4a3b4)) 
- *(type-infer)* :bug: Fix type applications not being added in places they should - ([2c50294](https://github.com/ElaraLang/elara/commit/2c502945c3e28bc337252b99594f2946bcee9bd4)) 
- *(type-infer)* :bug: Improve handling of skolems wrt generalisation - ([db45ce3](https://github.com/ElaraLang/elara/commit/db45ce3b06a2106f9e870c34817986c6222187e0)) 
- *(type-infer)* :bug: Ensure that _every_ type in the AST has the substitution applied - ([48acad9](https://github.com/ElaraLang/elara/commit/48acad921cc524e5d3423de868e3def6517b4ae1)) 
- *(type-infer)* :label: Fix type errors / missing Pretty instances - ([9350f9e](https://github.com/ElaraLang/elara/commit/9350f9edb1b4a496f8a26bd2d7ac15e890ec8ad4)) 
- *(type-infer)* :bug: Fix substitutions not being properly propagated - ([9aa552f](https://github.com/ElaraLang/elara/commit/9aa552f8e5653a88fbb6f8beac2400007f9278b5)) 
- *(type-infer)* :bug: Fix constraint generation for constructor patterns - ([35420e9](https://github.com/ElaraLang/elara/commit/35420e92c124eae0fbf4eb34b03f332e70ca8ac3)) 
- *(type-infer)* :bug: Fix constraint generation for constructor patterns sometimes leaving things unsolved - ([39b6c47](https://github.com/ElaraLang/elara/commit/39b6c47bebb2139a7580a5d5c6567101a3c8d8e7)) 
- *(type-infer)* :bug: Fix `ftv` implementation for Polytypes not working correctly - ([4828e97](https://github.com/ElaraLang/elara/commit/4828e97c421e8a37854ce9204a482e5c82fbe84c)) 
- Fix class names - ([4082462](https://github.com/ElaraLang/elara/commit/4082462f84f2507fa8ba4a06f8221bd4cc242192)) 
- Fix a possible bug in Stream.hs - ([7d25b00](https://github.com/ElaraLang/elara/commit/7d25b00d8bb20b32674d6365c20ae33b7fb1c164)) 
- Fix imports - ([cca1e0e](https://github.com/ElaraLang/elara/commit/cca1e0e71f8d52ba4a20658e1e5fa14ae51e0c1e)) 
- Fix constraint generation for variable references - ([5ceaaac](https://github.com/ElaraLang/elara/commit/5ceaaac51452c388d6b26cd7dba8b45816ca4c9e)) 
- Fix the instantiation descriptions - ([939ab7e](https://github.com/ElaraLang/elara/commit/939ab7e2fb753cdf63f721b38872e05725cee3d3)) 
- Fix nix hlint part3 - ([3752d52](https://github.com/ElaraLang/elara/commit/3752d52c32f790ddf5a7ddcc66391d05fd6be90f)) 
- Fix this stupid formatter once and for all - ([1601b02](https://github.com/ElaraLang/elara/commit/1601b02883aeb8c7e823c8a2f3afcc3be3f375ec)) 
- Fix bugs! - ([b19c315](https://github.com/ElaraLang/elara/commit/b19c315377c7231300db3b9e4a8f5698d0f54d68)) 
- Fix unit tests - ([a0b92bf](https://github.com/ElaraLang/elara/commit/a0b92bf437a82c9326f2cb1cfa62dd75c545a8bb)) 
- Fix dependency problem - ([22f8879](https://github.com/ElaraLang/elara/commit/22f8879b80a48a6d565d1203e589e9666cc8a821)) 
- Fix if/elses and datacons - ([056b4dc](https://github.com/ElaraLang/elara/commit/056b4dc9f6aa8d1598c06f3cf4f154bec5fc85f2)) 
- Fix compilation error - ([2ab710b](https://github.com/ElaraLang/elara/commit/2ab710b115a4767b751a734cf4d7cfbb00ee6abc)) 
- Fix the tests compiling - ([033b41c](https://github.com/ElaraLang/elara/commit/033b41c3a6b39e00e429516b37eec4192c4afc59)) 
- Fix the tests! - ([5a3a136](https://github.com/ElaraLang/elara/commit/5a3a136477ddf3433e81d647c0345f3839767c0a)) 
- Fix more tests - ([076711a](https://github.com/ElaraLang/elara/commit/076711a6984ad4968634fcaeabfc9d91218301f7)) 
- Fix Desugarer bug - ([3237f72](https://github.com/ElaraLang/elara/commit/3237f72164a340613333c46984248e5530e66387)) 
- Fix some warnings - ([c0dfad1](https://github.com/ElaraLang/elara/commit/c0dfad1807c990aa9bba2a2965bd760db351d723)) 
- Fix tests not compiling - ([ed573eb](https://github.com/ElaraLang/elara/commit/ed573ebcbae13510698fe8b3faac55b1c003d8a0)) 
- Fix test compilation - ([86c98ce](https://github.com/ElaraLang/elara/commit/86c98cecdd32711d750d8f4a05a745add63e034b)) 
- Fix tests compiling again - ([6df8797](https://github.com/ElaraLang/elara/commit/6df8797f1da0a458c71eafa777f84feca34d2827)) 
- Fix generic prettying of selectors - ([be53bff](https://github.com/ElaraLang/elara/commit/be53bff813fc5df71a34fafcc2ebb99800913569)) 
- Correct information about bool type constructor - ([b9ea2ed](https://github.com/ElaraLang/elara/commit/b9ea2ed7da1de90fc5c4a72d1a6ee27f1af1abb8)) 
- Fix unique implementation having a questionable Eq/Ord Instance - ([162734a](https://github.com/ElaraLang/elara/commit/162734a197fb2efa837007ef7fcc70f6c91c2ca7)) 
- Make getInputFiles actually work lol - ([4c08eb2](https://github.com/ElaraLang/elara/commit/4c08eb290b46803b8c4b949df0b64aa973f8c181)) 

### 🚜 Refactor

- *(codegen)* :lipstick: Add parentheses to toString generation - ([6e35e1b](https://github.com/ElaraLang/elara/commit/6e35e1be79ab7a74c3032b76e9d6d7c68c5a6465)) 
- *(codegen)* :lipstick: Improve pretty instance for NamedMethodDescriptor - ([6b5e843](https://github.com/ElaraLang/elara/commit/6b5e843a3c474011a07357fcd92d059eb11e8030)) 
- *(codegen)* :loud_sound: Use StructuredLog in codegen - ([e65670a](https://github.com/ElaraLang/elara/commit/e65670a4dd42ff9350406c349455a305a9e2a3da)) 
- *(codegen)* :loud_sound: Improve the logging a little bit by removing some of the now-redundant "X Complete" logs - ([61747db](https://github.com/ElaraLang/elara/commit/61747db9f98cab969e76faeff21adce3caa72b24)) 
- *(core)* :recycle: Rename `exprType` to `guesstimateExprType` - ([299ea21](https://github.com/ElaraLang/elara/commit/299ea2137e7dbfca2f40843f50a68d0305d30fd3)) 
- *(core)* :mute: Remove old debug output from TypeCheck.hs - ([1e5a7f7](https://github.com/ElaraLang/elara/commit/1e5a7f759a294966afc52915ef143aac62a7a692)) 
- *(core)* Move most of the core process to the query system - ([efd7dfb](https://github.com/ElaraLang/elara/commit/efd7dfb813a46ec768ca0bcfecd7721548fe9cc3)) 
- *(desugarer)* Move desugarer to be query based - ([8d0c89e](https://github.com/ElaraLang/elara/commit/8d0c89e7d5137cebf553babe8ee811652ba0bdab)) 
- *(desugarer)* :technologist: Improve DefWithoutLet error formatting - ([faa24bd](https://github.com/ElaraLang/elara/commit/faa24bde4b84afe8862e219dd593c71127b1a2c8)) 
- *(errors)* Make error reporting a bit more flexible - ([2e08bf5](https://github.com/ElaraLang/elara/commit/2e08bf594e3d8bdc8cc862680ee8376adb8b9691)) 
- *(errors)* Update ReportableError class to improve default error handling - ([846f6eb](https://github.com/ElaraLang/elara/commit/846f6ebee48b340fd20f029e255ea261bbcb276d)) 
- *(interpreter)* Move interpreter to query system! - ([82189c3](https://github.com/ElaraLang/elara/commit/82189c302cd82e00eef42af52299187791c61455)) 
- *(lexer)* :recycle: Change lexer to distinguish between manual and automatic semicolon tokens - ([613783b](https://github.com/ElaraLang/elara/commit/613783b4ff953f032e735c18020e2d8577e9295a)) 
- *(logging)* :recycle: Extract a function `defaultReport` providing a usable default `ReportableError` implementation - ([ad408da](https://github.com/ElaraLang/elara/commit/ad408da87c24da6a79c2e79bfe2430d62ebcbb16)) 
- *(logging)* :loud_sound: Make sure that core dumps _always_ happen even if typechecking fails - ([ba1bab2](https://github.com/ElaraLang/elara/commit/ba1bab2f4683d5df8691cadb45d0b2e81f507e11)) 
- *(logging)* :loud_sound: Further improve core dump logs on errors - ([02d4a0b](https://github.com/ElaraLang/elara/commit/02d4a0b7b60cbdfaa38348dc1432585b79139542)) 
- *(main)* Re-implement all --dump-X flags - ([eb6613a](https://github.com/ElaraLang/elara/commit/eb6613a16e34f540effe85397a49356f9d15fedb)) 
- *(nix)* :building_construction: Move from haskell-flake to hix - ([fe16f77](https://github.com/ElaraLang/elara/commit/fe16f77513eea306bee9673db4e4ed0664e4e3b2)) 
- *(parser)* :mute: Remove debug logging from parser - ([61b6c3c](https://github.com/ElaraLang/elara/commit/61b6c3c1d514a837952c7ab73489c6177a01f382)) 
- *(renamer)* :recycle: Desugar tuple literals into normal data constructors - ([9452cc7](https://github.com/ElaraLang/elara/commit/9452cc7e5bd7fc762be6c932621f2b1bafcc30e0)) 
- *(renamer)* Move renamer to query based system. This means the entire compiler can be lazy! - ([65c1d7a](https://github.com/ElaraLang/elara/commit/65c1d7ac8276f445be264acc82d66213a0e73cd8)) 
- *(shunt)* Tidy up code - ([11d08a9](https://github.com/ElaraLang/elara/commit/11d08a94fc4d48b9b065c58b406331c81c7b25fc)) 
- *(shunter)* Refactor shunter and significantly clean up other code - ([4c1d3a8](https://github.com/ElaraLang/elara/commit/4c1d3a8a986038645e71aad098ef7b2b9b381a3c)) 
- *(stdlib)* Use nicer syntax in string.elr - ([7e0374a](https://github.com/ElaraLang/elara/commit/7e0374ad1e3248154ca871c571545496be5a4f4d)) 
- *(to-core)* :loud_sound: Improve error messages in Core.Analysis.exprType - ([86e4f40](https://github.com/ElaraLang/elara/commit/86e4f40791f385010615dccc84367e775184f7db)) 
- *(to-core)* Move to-core to query system - ([cc65b7b](https://github.com/ElaraLang/elara/commit/cc65b7b9193cab3d7769dfaab94ef85fd7d3f1f2)) 
- *(type-check)* Improve type checking for non-recursive lets and enhance error reporting - ([f6dcef1](https://github.com/ElaraLang/elara/commit/f6dcef1e6cad5fb571e0e52be5f345363ff3c354)) 
- *(type-infer)* Improve debug output - ([18aa644](https://github.com/ElaraLang/elara/commit/18aa644db55440f2a7a2306e1c4a519f3e99d467)) 
- *(type-infer)* Enhance type inference by implementing skolemisation for expected types - ([c7cb69a](https://github.com/ElaraLang/elara/commit/c7cb69abd5004f44b128bcfa580f9dd6d51464f6)) 
- *(type-infer)* Move type inference to query system (still semi-wip) - ([ec9a57c](https://github.com/ElaraLang/elara/commit/ec9a57c313ab38fb848a7797b95ad2da751f65a8)) 
- *(type-infer)* Add some location information to type infer process for better debugging (still not very good) - ([091e6da](https://github.com/ElaraLang/elara/commit/091e6dafb02e8e1934bc0ae1981f89d80132756b)) 
- :loud_sound: Refactor structured (i.e. potentially nested) logging into its own effect - ([6492cb2](https://github.com/ElaraLang/elara/commit/6492cb266d6d3c2ffa8603fb201a985b221039b3)) 
- :recycle: Tidy up a lot of the generic AST code, including removing usages of `unsafeCoerce` - ([a58941f](https://github.com/ElaraLang/elara/commit/a58941f10fafe83ee204c9ba89e89cec904c642d)) 
- :recycle: More tidying - ([b73678b](https://github.com/ElaraLang/elara/commit/b73678b2bca855dbabe0b4338fa41f63a0ac521f)) 
- Refactor typed ast to only store monotypes - ([a5afa80](https://github.com/ElaraLang/elara/commit/a5afa80022e642fa5036b63e788168b77d2b36de)) 
- Refactor ast naming to be "simpler" and make more invalid states unrepresentable - ([af37b2b](https://github.com/ElaraLang/elara/commit/af37b2b09b206014692fc158e047c00bb3099f5b)) 
- Refactor the logging to be a bit cleaner - ([a0faa0a](https://github.com/ElaraLang/elara/commit/a0faa0af349131a465e6ddfbb3c0f17c2b633ab2)) 
- :recycle: Tidy up VarRef, removing dead functions and introducing more pattern synonyms - ([6e85f1a](https://github.com/ElaraLang/elara/commit/6e85f1a93b8d468adbf057f0fed58124faddeaf0)) 
- Refactor + and - primops - ([a6d6831](https://github.com/ElaraLang/elara/commit/a6d6831a4e5fa460d8696f6361630301c8767ec8)) 
- :loud_sound: Change priority of pretty printing `ValueType`s - ([fd4070b](https://github.com/ElaraLang/elara/commit/fd4070b30ce7db4811c777170477d64af14d0fcd)) 
- Minor tidy up - ([51a4f42](https://github.com/ElaraLang/elara/commit/51a4f425f4340eef2d3baa01d22d5acd434d12c1)) 
- :recycle: Start refactor to Rock query system! - ([4dacd0f](https://github.com/ElaraLang/elara/commit/4dacd0f39b32ab6940a4e5c9f4bdaeb41d11b858)) 
- Start moving over to rock system (and remove internal uses of 'String' in lexer) - ([de8dd24](https://github.com/ElaraLang/elara/commit/de8dd24b56e485f8859a6897e82205b58f73d83b)) 
- Move parser to query based system - ([976bbfa](https://github.com/ElaraLang/elara/commit/976bbfa579dd64a218fc0da3cb2148b50f3cd429)) 
- :coffin: Remove old polysemy pipeline stuff - ([f49ded8](https://github.com/ElaraLang/elara/commit/f49ded89be08cb53b9a6ac4f3b9aa55c3b565a1e)) 
- Add files to diagnostics again when they're read - ([22d481c](https://github.com/ElaraLang/elara/commit/22d481c9a514f0dbc7770b7c0f086bec70eda4dc)) 

### 📚 Documentation

- *(core)* :memo: Slightly improve Core.Type documentation - ([ef5e95f](https://github.com/ElaraLang/elara/commit/ef5e95f4d49420fe93cf6f56ff41e6be3d155c6d)) 

### 🎨 Styling

- *(core)* :lipstick: Improve Core prettyprinter - ([fb8a911](https://github.com/ElaraLang/elara/commit/fb8a911cd8821f702e1432da3ae8ae05b919a0c7)) 
- *(core)* :art: Only import pretty instances - ([61209f1](https://github.com/ElaraLang/elara/commit/61209f1330e95f04dfec3bf55968fd22eed63526)) 
- *(core)* :art: Tidy up ToANF a little - ([d65a497](https://github.com/ElaraLang/elara/commit/d65a497aeacff1b91370e6073f140a7fd5941f77)) 
- *(core)* :art: Add the "of" to dumped core case expressions - ([bbd5c43](https://github.com/ElaraLang/elara/commit/bbd5c4357175a933a53a177c82a2943819ab7a2b)) 
- *(core)* :art: Remove some of the printing of variable types from core dumps - ([0a7e493](https://github.com/ElaraLang/elara/commit/0a7e493b7101f2f1d9a28afef56cdfe4bcd0bc1c)) 
- *(core)* :art: Adjustments to lambdas in core printing - ([d5be790](https://github.com/ElaraLang/elara/commit/d5be790a31ba0edabe77fb31ff0cb9d4240969d1)) 
- *(core)* :lipstick: Improve Core Typecheck error message - ([7caae66](https://github.com/ElaraLang/elara/commit/7caae66615e2175f0e1c2e867b04c761f6a395f3)) 
- *(core)* :lipstick: Change prettyprinting of Core let rec - ([62007ef](https://github.com/ElaraLang/elara/commit/62007efd22e336812b017e6ffbc86f2230c061fe)) 
- *(core)* :art: Remove the annoying spacing in core dumps after `=` - ([23b0157](https://github.com/ElaraLang/elara/commit/23b015785da167fad38f4da70028387fbad5207a)) 
- *(core)* :art: Improve core prettyprinting a lot - ([8b9ecaa](https://github.com/ElaraLang/elara/commit/8b9ecaaf6896dfeb1c5fa025e41da17299cc7c22)) 
- *(interpreter)* :loud_sound: Only log the _new_ bindings when interpreting a module - ([b5ab05e](https://github.com/ElaraLang/elara/commit/b5ab05e92ec2245ca8405daf49468ede57dddc31)) 
- *(lexer)* :pencil2: Fix typo - ([0401dfe](https://github.com/ElaraLang/elara/commit/0401dfe99c588eae8c02b84bf5153bbe0df7f8d5)) 
- *(lexer)* :pencil2: Fix typos in lexer rules names - ([9601045](https://github.com/ElaraLang/elara/commit/9601045e81d04c1e49b83640ae3dbf6e480585d5)) 
- *(lexer)* Tidy lexer and remove debugging logs - ([72b804e](https://github.com/ElaraLang/elara/commit/72b804e501a984049cfe55990d413f5cf834cf33)) 
- *(parser)* :mute: Remove debug logging from parser - ([5878aa7](https://github.com/ElaraLang/elara/commit/5878aa7a760e35bde772a008bae76bf1dde84224)) 
- *(tests)* Make test function slightly more readable - ([0841c00](https://github.com/ElaraLang/elara/commit/0841c00935f2ceaebca6faf459001e1ee1858307)) 
- *(to-core)* :mute: Remove old debug message - ([eda6d02](https://github.com/ElaraLang/elara/commit/eda6d02197fd1e57653362d2960cd8e36ded82ec)) 
- *(to-core)* :loud_sound: Improve debug logging for ToCore - ([7c5e6dc](https://github.com/ElaraLang/elara/commit/7c5e6dc0691e371720d1c301bb601cf492fc7496)) 
- *(to-core)* :art: Format code and add some debug logging - ([14d19f7](https://github.com/ElaraLang/elara/commit/14d19f798c01a94e54194c102d0940f76c7e606f)) 
- :art: Format everything with treefmt - ([7f065d1](https://github.com/ElaraLang/elara/commit/7f065d121d8329569354e363c853febb87bef100)) 
- :art: Format everything with hlint - ([d7f0fd5](https://github.com/ElaraLang/elara/commit/d7f0fd5b9f66efb341184ab4d902af33a84322ef)) 
- :art: Format everything with fourmolu - ([4d1cf57](https://github.com/ElaraLang/elara/commit/4d1cf57028c83d21ca65efa18d0db7f66e2b4dad)) 

### 🧪 Testing

- *(parser)* :white_check_mark: Improve tests of parser and lexer - ([5aad58c](https://github.com/ElaraLang/elara/commit/5aad58cb93b148eadc1fae34a1a5cee92a6ce966)) 
- *(type-infer)* :white_check_mark: Improve behaviour of applicableTyApp and test cases - ([06ad585](https://github.com/ElaraLang/elara/commit/06ad585f6d548bef257ea10eccd592394bd21e14)) 
- :bug: Fix unit test compilation! - ([cefd2aa](https://github.com/ElaraLang/elara/commit/cefd2aa1ed624e7ce29f4f3370e43011cd99fc35)) 

### ⚙️ Miscellaneous Tasks

- :fire: Remove old unused Grammar directory - ([f7d2f31](https://github.com/ElaraLang/elara/commit/f7d2f3182d7a9723102baf503b5950d64346c8c8)) 

### ◀️ Revert

- *(prim)* Revert those prim changes which were definitely not correct - ([946fa0b](https://github.com/ElaraLang/elara/commit/946fa0bd3041e1bccaac3aef10a82e98d97d864c)) 

### Build

- *(logging)* :bug: Update polysemy-log dependency, maybe fixing some compilation issues - ([a32ba45](https://github.com/ElaraLang/elara/commit/a32ba45aabeeca0c16c0838f4f10f881dfc7637e)) 
- *(nix)* :heavy_plus_sign: add rock dependency - ([568d228](https://github.com/ElaraLang/elara/commit/568d2284021d23046bc5a2f538f11dfe5686717c)) 
- :arrow_up: Update to GHC 9.10.1 - ([10db81c](https://github.com/ElaraLang/elara/commit/10db81cabb072cfe461638a97b313b10d470a574)) 
- :building_construction: Make build flags consistent between tests and executables - ([997f2f1](https://github.com/ElaraLang/elara/commit/997f2f1d680e468315c4cf4a29068c0abdee816e)) 
- :arrow_up: Update `optics` - ([eb35403](https://github.com/ElaraLang/elara/commit/eb3540318dbbd1a33081c34d538d2c360a68adcb)) 
- :label: Add a type signature to `transformOf'` - ([d9e50db](https://github.com/ElaraLang/elara/commit/d9e50dbaa8922fa9f6c5358fe6a78a4135e05aad)) 
- :arrow_up: Fix dependencies in Nix and use GHC 9.12 - ([e2a6322](https://github.com/ElaraLang/elara/commit/e2a63223f8833e5a468120b429e7576ca8de081a)) 
- Update cabal file - ([a50ca82](https://github.com/ElaraLang/elara/commit/a50ca82aeacd55c8d373073d836e170fb31599d2)) 

### Unfinished

- Move type inference to query system - ([69ba570](https://github.com/ElaraLang/elara/commit/69ba570b300d9928d6cd73c700f67db5ac473922)) 

## [0.1.0](https://github.com/ElaraLang/elara/compare/v0.0.2..v0.1.0) - 2024-06-01

### 🚀 Features

- *(renamer)* :bug: Fix #14 and significantly improve error message quality for unknown names - ([d22a9b5](https://github.com/ElaraLang/elara/commit/d22a9b59bdc87a5bbb11a350b95d275d5eae6ab2)) 
- *(renamer)* :lipstick: Use code E0001 for unknown name again - ([8e4d3c0](https://github.com/ElaraLang/elara/commit/8e4d3c07679613ef388837ab2b20db781f7d623a)) 

### 🐛 Bug Fixes

- *(renamer)* :rotating_light: Fix invalid import - ([9162419](https://github.com/ElaraLang/elara/commit/916241968cff7a92feea3441a56161212d308f9e)) 

### 🚜 Refactor

- *(renamer)* :recycle: Change renamer to have the current module be _optional_ - ([515f619](https://github.com/ElaraLang/elara/commit/515f6197597b15760811171b8cbb4f92f607fd07)) 

### 📚 Documentation

- *(changelog)* Update CHANGELOG.md - ([31c62a7](https://github.com/ElaraLang/elara/commit/31c62a74c4273f360f1b8cbb88a57c166ae942c9)) 
- *(changelog)* Use the git-cliff format for current changelog - ([f52b80b](https://github.com/ElaraLang/elara/commit/f52b80b26ba811767b4c0e4cc7059b8c24df8601)) 
- *(changelog)* Use the git-cliff format for current changelog - ([9356157](https://github.com/ElaraLang/elara/commit/9356157d672d7c5f2e402e835a0947e25cf32d03)) 

### 🎨 Styling

- *(renamer)* :rotating_light: Remove unused variable binding - ([bbd9441](https://github.com/ElaraLang/elara/commit/bbd9441fda5ca16c4e49e73f6fe10eb1e838f688)) 
- *(renamer)* :lipstick: Slightly reword unknown name error message - ([8936a7c](https://github.com/ElaraLang/elara/commit/8936a7c61bf3c4039f63bbd62295716621447c9d)) 

### 🧪 Testing

- *(shunter)* Fix shunting tests no longer compiling - ([4e9d196](https://github.com/ElaraLang/elara/commit/4e9d196c4ab64695f7c0c2440abf5a90ddfa8f8d)) 

### ⚙️ Miscellaneous Tasks

- *(cd)* Add action to generate changelogs whenever you push to main - ([cea8bae](https://github.com/ElaraLang/elara/commit/cea8bae0ec2d7fba47b7831ac720f8cc21fce308)) 
- *(cd)* Add action to generate changelogs whenever you push to main - ([bdbcfb9](https://github.com/ElaraLang/elara/commit/bdbcfb900923116c535d046a99b1d1f6d4323864)) 
- *(changelog)* Add release script - ([8dca1bb](https://github.com/ElaraLang/elara/commit/8dca1bb1d6a90ac20d867fb056d54ac809d0b844)) 

### Build

- *(changelog)* Add cliff.toml - ([682cc9f](https://github.com/ElaraLang/elara/commit/682cc9f771b49de35f2f4d70f4829d2e4bd31523)) 
- *(changelog)* Add cliff.toml - ([b498ae3](https://github.com/ElaraLang/elara/commit/b498ae36aaad575f2f375596fc67f8b6b4cfe713)) 
- *(changelog)* Add a safety check to the release script and allow aborting it - ([c1ec919](https://github.com/ElaraLang/elara/commit/c1ec91978c06e527ea1a665f490c24e3380a2f1f)) 
- *(changelog)* :bug: Add more safety checking to release.sh and fix weird sed behaviour - ([1d01db1](https://github.com/ElaraLang/elara/commit/1d01db166fb361126fe4e0cf087342d39a5bdd5d)) 
- *(changelog)* :art: Change changelog generation for tags - ([536e580](https://github.com/ElaraLang/elara/commit/536e5809c4111fa3a1c72b8bf5ee386f5f949f5d)) 
- *(changelog)* :safety_vest: Ask if we're ok before making git tags - ([3e3e457](https://github.com/ElaraLang/elara/commit/3e3e457b238d94b61387187bbe7ddf2eb1e27f58)) 
- *(nix)* Add `git-cliff` to devshell - ([8a6187e](https://github.com/ElaraLang/elara/commit/8a6187e4038f9dc73e756983c6abd9aaec4eb4d9)) 
- *(nix)* Add `git-cliff` to devshell - ([7b5a372](https://github.com/ElaraLang/elara/commit/7b5a372ad3cfccfdbf9c781422926c6f8de4df22)) 

## [0.0.2](https://github.com/ElaraLang/elara/compare/v0.0.1..v0.0.2) - 2024-06-01

### 🚀 Features

- *(renamer)* Add a nice pretty error report for when the renamer catches ambiguous names - ([f31a6cd](https://github.com/ElaraLang/elara/commit/f31a6cd94be1e86fcd419fcc2d0a47b79e08e086)) 

### 🐛 Bug Fixes

- *(renamer)* Fix heavy-handed ambiguous name checking reporting ambiguity when there actually isn't any! Fixes variable shadowing - ([ecb3481](https://github.com/ElaraLang/elara/commit/ecb3481cd8580e8b1ac9cfca42413f0996e5ad81)) 

### 🚜 Refactor

- *(renamer)* Change renaming backend to allow multiple names with the same unqualified name - ([f61f155](https://github.com/ElaraLang/elara/commit/f61f155c4f2c57e069ff82fa1cf7149d69d0dbdf)) 

### 📚 Documentation

- *(changelog)* Update CHANGELOG.md - ([0882b33](https://github.com/ElaraLang/elara/commit/0882b339938554cd083a197b1a4b7092f23b905c)) 

## [0.0.1](https://github.com/ElaraLang/elara/compare/v0.0.0..v0.0.1) - 2024-06-01

### 🚀 Features

- *(stdlib)* Load all the stdlib files dynamically based on what's in the new stdlib folder - ([47e0e90](https://github.com/ElaraLang/elara/commit/47e0e90b71f41d830fe269f7cb99224ef7c0b1b9)) 
- *(stdlib)* Add Option module to stdlib - ([646f1ae](https://github.com/ElaraLang/elara/commit/646f1aefa5b2e6d9ef88c4401acfac24b91369e7)) 
- *(tocore)* Improve reporting of `UnknownConstructor` error in ToCore - ([8fda8b3](https://github.com/ElaraLang/elara/commit/8fda8b31a101f0e70c70e5cdd21ae9967f475a66)) 

### 🐛 Bug Fixes

- *(tocore)* Fix ToCore not being invoked in topological order, causing unknown name errors - ([8737b28](https://github.com/ElaraLang/elara/commit/8737b281c63b8b59055d0c3d1d319f8c31a41119)) 

### 🚜 Refactor

- *(stdlib)* Move all stdlib files into new folder - ([e7cd134](https://github.com/ElaraLang/elara/commit/e7cd1344aa8c04dafc79c2ff8f537985cc62708c)) 

### 🎨 Styling

- *(actions)* Remove blank line - ([20b7d8a](https://github.com/ElaraLang/elara/commit/20b7d8a958a29db2a324fc8987980873f8270807)) 
- *(stdlib)* Make main file a bit more fully-fledged - ([32b919c](https://github.com/ElaraLang/elara/commit/32b919c8358bb844b74228c2148dac3cd53a94d0)) 

### ⚙️ Miscellaneous Tasks

- *(actions)* Try deleting this folder - ([c35171c](https://github.com/ElaraLang/elara/commit/c35171c71d049f4b468ceedea39f97c8407cbcaf)) 
- *(actions)* Change the script...? - ([4ab2c8f](https://github.com/ElaraLang/elara/commit/4ab2c8fa44b3aa0f910bced1b8e663c853594d99)) 
- *(actions)* Run linux builds on self-hosted runner - ([9f8f291](https://github.com/ElaraLang/elara/commit/9f8f291e6e3dab4d50b3682a02e9b7a0c2c6fb42)) 
- *(actions)* Nevermind let's not do that - ([a30d954](https://github.com/ElaraLang/elara/commit/a30d954a686e116764c127e77b0b9d228af40a3c)) 
- *(actions)* Log big files - ([282fb34](https://github.com/ElaraLang/elara/commit/282fb34aec3685f45f1a086d3a4fc418a1412624)) 
- *(actions)* That seemed to work - now remove the logging - ([31478e6](https://github.com/ElaraLang/elara/commit/31478e679894b86feb899a8fa50e3b18babde244)) 

## [0.0.0] - 2024-06-01

### 🚀 Features

- *(logging)* Determine the log level based on elaraDebug variable - ([93f8f8a](https://github.com/ElaraLang/elara/commit/93f8f8aefbe115ccd7bedaac3a404d15e3ececce)) 
- *(pretty-printing)* Improve colourisation of pretty-printer output - ([28492c5](https://github.com/ElaraLang/elara/commit/28492c57370c98ea8142574261dc36d1b80185b9)) 
- *(pretty-printing)* Remove the domain when printing foralls (as it will only ever be type) - ([2b5158e](https://github.com/ElaraLang/elara/commit/2b5158e3eb9459fb67079669025794a7fa81910a)) 
- *(stdlib)* Restore the stdlib to its former glo - ([0fc83e9](https://github.com/ElaraLang/elara/commit/0fc83e918b2ecf1422cf08c226d7f49442f175b7)) 
- *(stdlib)* Add primitive functions for working with strings - ([a582f06](https://github.com/ElaraLang/elara/commit/a582f0619b44bd20fc8fbbcfef4c744eb06a58fb)) 
- *(type-infer)* Add a new error message in the case of multiple tyapps being detected (which shouldn't happen?) - ([7486ea1](https://github.com/ElaraLang/elara/commit/7486ea12c758a85334ff48556be6365bc63302fa)) 

### 🐛 Bug Fixes

- *(codegen)* Fix stringLength primitive not boxing integers - ([85fcf5a](https://github.com/ElaraLang/elara/commit/85fcf5a6e72d2cd27f68c0854de3bf54e7b54b80)) 
- *(logging)* Remove old debugPretty calls - ([64edc03](https://github.com/ElaraLang/elara/commit/64edc0310f539740bc81138b6b042b9333501617)) 
- *(tests)* Fix unit tests not compiling for various reasons - ([e0a2955](https://github.com/ElaraLang/elara/commit/e0a2955d618e7a6ea1d244d2cf4672fb476c51ed)) 
- *(type-infer)* Fix constructor pattern arguments being reversed in type inference - ([321fdd6](https://github.com/ElaraLang/elara/commit/321fdd6fbe6aaad25a547006d753a97f616e18d9)) 
- *(type-infer)* Fix conversion between types when explicitly applying type constructors (eg `def x : Option Int`) - ([940e960](https://github.com/ElaraLang/elara/commit/940e9608a699d06b9324f169cc1367afc43907d9)) 
- *(type-infer)* Fix constructor patterns not being completely solved - ([1dc7624](https://github.com/ElaraLang/elara/commit/1dc7624f1dbaeab084a4fb7e3ae23170eb92337a)) 
- *(type-infer)* Fix type inference logging _always_ being on debug - ([3b34bb1](https://github.com/ElaraLang/elara/commit/3b34bb1e216fafad98cf08c1d0dec4f242b61587)) 
- *(type-infer)* Fix if/else type inference not always unifying correctly and throwing errors - ([33a72c2](https://github.com/ElaraLang/elara/commit/33a72c2103a1657dc9626176c2eb683513e9d4bf)) 
- *(type-infer)* Improve algorithm for detecting tyapps, fixing some edge cases - ([8503c23](https://github.com/ElaraLang/elara/commit/8503c23109a8f79180a70b77393040141d6527df)) 
- Fix the tests - ([1273624](https://github.com/ElaraLang/elara/commit/1273624f5efd8a4dc9a52df073e199c03a180874)) 
- Fix name shadowing in inspection - ([56dc353](https://github.com/ElaraLang/elara/commit/56dc3534dfa22b5aee0582788784570c7560486e)) 
- Fix haddock comment - ([945cd39](https://github.com/ElaraLang/elara/commit/945cd398eaccb880d36017ba8f06279ba93880ae)) 
- Fix latex - ([57cb4ac](https://github.com/ElaraLang/elara/commit/57cb4ac62336eea9cddb65e24c730b70d3533801)) 
- Fix a bunch of warnings :D - ([bef63d7](https://github.com/ElaraLang/elara/commit/bef63d70b55ed5e988bd822513df2ed95c44bdde)) 
- Fix some more warnings - ([ea6b295](https://github.com/ElaraLang/elara/commit/ea6b295a211c57609665f2c23833578738d485a2)) 
- Fix more warnings - ([877fa2d](https://github.com/ElaraLang/elara/commit/877fa2df71692775c9cba46f92c361fae38b747b)) 
- Fix a naughty parser edge case, but now indentation is broken - ([fb64659](https://github.com/ElaraLang/elara/commit/fb646599f65db122aa30e5cea8a250e055de5429)) 
- Fix local let bindings not being annotated properly - ([5437b1b](https://github.com/ElaraLang/elara/commit/5437b1bc0f789b89090689919a0cda3e3f873684)) 
- Fix parsing tests, make let-binding names unqualified - ([16221a2](https://github.com/ElaraLang/elara/commit/16221a2b96fd9ddf7dc6e80b1523c8ae08376253)) 
- Fix funny quickcheck edge cases - ([c231e2a](https://github.com/ElaraLang/elara/commit/c231e2ac6872efe1d4aac5590087f6f01c0ea446)) 
- Fix some haddock typos - ([543a982](https://github.com/ElaraLang/elara/commit/543a982fb6c6c2bb9e288465ace248b64eb73ba0)) 
- Fix let-in error - ([9b60ec8](https://github.com/ElaraLang/elara/commit/9b60ec83ea45e08cfd022832c64c890a0f748a88)) 
- Fix prettyprint infinite loop lol - ([ace5c19](https://github.com/ElaraLang/elara/commit/ace5c19e65adf51e119efa3af67a99eaa8addc66)) 
- Fix unification with type declarations - ([24e2278](https://github.com/ElaraLang/elara/commit/24e2278773432462e072d1e412e530ac30d52026)) 
- Fix da nix - ([7ed19ef](https://github.com/ElaraLang/elara/commit/7ed19ef155b2f56e03544e06de7bdef3ac6aeee4)) 
- Fix da stack (doesnt rhyme sorry) - ([79018aa](https://github.com/ElaraLang/elara/commit/79018aabb9594ee3aef344f8b92c03fbba2b1583)) 
- Fix type inference? - ([b05c98a](https://github.com/ElaraLang/elara/commit/b05c98a9ff0afeabb47155c88ab6c1643253c3fd)) 
- Fix hie - ([2b53f88](https://github.com/ElaraLang/elara/commit/2b53f88ee07827d67cf0c7ccce5520ffeb38b7bc)) 
- Fix unit tests - ([2ef17dd](https://github.com/ElaraLang/elara/commit/2ef17dd60ebfabbec9f05a7b67fcfb2528e02db2)) 
- Fix doctests - ([f2543c7](https://github.com/ElaraLang/elara/commit/f2543c7bd40427ad91488df1dc959d6df58c8774)) 
- Fix the pattern parser & add a lot of tests - ([94d1669](https://github.com/ElaraLang/elara/commit/94d1669fa7c151d4d36964fa3a05ca185173cb1b)) 
- Fix lots of bugs! - ([438e69f](https://github.com/ElaraLang/elara/commit/438e69f744c68b730aa625e891e593d57a5aaff7)) 
- Fix subtyping on custom types - ([c790d2f](https://github.com/ElaraLang/elara/commit/c790d2f756aef3386a5f54ff8fd32898f11dbd27)) 
- Fix stupid ugly CI (maybe) - ([ad7a872](https://github.com/ElaraLang/elara/commit/ad7a87227ce7a506a5903c228ea390f17fb517e7)) 
- Fix some warnings and refactor even more - ([1d96b4d](https://github.com/ElaraLang/elara/commit/1d96b4d7d96379084a914f3d8090bf4de9078be1)) 
- Fix a morbillion more warnings - ([be1c891](https://github.com/ElaraLang/elara/commit/be1c891aadf9850bb817aa0aaca5216b1ba6568b)) 
- Fix almost all the warnings - ([351f7fb](https://github.com/ElaraLang/elara/commit/351f7fb55ed24254401eb88894cc790719a3069d)) 
- Fix a very rare lexer edge case - ([b0369fc](https://github.com/ElaraLang/elara/commit/b0369fcc435934d20876ae0dd08e0c165acc8e11)) 
- Fix build error - ([c154b25](https://github.com/ElaraLang/elara/commit/c154b25657399e75d675761e97f0baf597714dbb)) 
- Fix the tests not compiling (oops) - ([4a620ee](https://github.com/ElaraLang/elara/commit/4a620ee9f3549ce4592ca06a88cfe8f57b9497a4)) 
- Fix the tests not compiling (oops) - ([4ff8661](https://github.com/ElaraLang/elara/commit/4ff8661e822452390a0ebc402a7055c152a638f8)) 
- Fix let-in constructs being given the wrong type in core - ([e52396e](https://github.com/ElaraLang/elara/commit/e52396e44274be57da41a52fe7189040cbd40a57)) 
- Fix maxLocals for <clinit> - ([1a09f30](https://github.com/ElaraLang/elara/commit/1a09f30c220e563ca60d2d610e7b7ab65838a580)) 
- Fix bug in renaming let statements - ([4825d10](https://github.com/ElaraLang/elara/commit/4825d102860a0a61c8dae696875b42cff7eb6c44)) 
- Fix the cons type - ([8813151](https://github.com/ElaraLang/elara/commit/8813151a17505b3224514a429c93df1220e03302)) 
- Fix parse error with higher order functions - ([4452451](https://github.com/ElaraLang/elara/commit/44524515c89c7a84dd3578228fee0c609a539b04)) 
- Fix unnecessary type applications being added - ([09dc4ba](https://github.com/ElaraLang/elara/commit/09dc4ba5e96d93dc09cebe79a170815b0c479d4a)) 
- Fix variables in ToCore using the wrong types - ([d3da4bb](https://github.com/ElaraLang/elara/commit/d3da4bb60b3ee3e376b19939fa067a85819aae95)) 
- Fix (?) type inference for list literals - ([8a20d85](https://github.com/ElaraLang/elara/commit/8a20d856748c3472b488fa719b3c117074a5b07f)) 
- Fix CI - ([34bbcb0](https://github.com/ElaraLang/elara/commit/34bbcb0fe09c74121a723c2da76c6aff86e3887c)) 
- Fix undefined handling - ([85a0df4](https://github.com/ElaraLang/elara/commit/85a0df48b54f9cb6e76ff487150f840d0a4d1cdc)) 
- Fix |> and emitting higher order functions - ([69b3236](https://github.com/ElaraLang/elara/commit/69b3236708023b15ea9aed0b011d324bf7990ae5)) 
- Fix more tyapps - ([70aae06](https://github.com/ElaraLang/elara/commit/70aae06edb2a4ea2ebdcc31707532b1a68624ff3)) 
- Fix redundant pattern match warning with Local' & Global' - ([cd0ca0c](https://github.com/ElaraLang/elara/commit/cd0ca0c8b7982a9083c20ea2adfbfed25ad1a331)) 
- Fix core type instantiation being incorrect - ([97427ab](https://github.com/ElaraLang/elara/commit/97427ab1a7731e19bb4ccec1170c5f4350158e0a)) 
- Fix match expressions inferring to the wrong type - ([ad97cef](https://github.com/ElaraLang/elara/commit/ad97cefc9f91e5385977692e78b78b83974825d1)) 
- Fix ci running - ([a858457](https://github.com/ElaraLang/elara/commit/a8584571399ee3ff27f7fcae80546b012a5945eb)) 
- Fix shunt tests - ([d017534](https://github.com/ElaraLang/elara/commit/d017534f7c17767811bd2663d4167d2b8a30b782)) 
- Fix a bunch of warnings - ([aa811c7](https://github.com/ElaraLang/elara/commit/aa811c7ccc41b085aa58a78cdeb59bfbf57762fe)) 
- Fix the unit tests compiling - ([b2e838d](https://github.com/ElaraLang/elara/commit/b2e838dc9ee35e6a9f8abb7a9a01064904d1c682)) 
- Fix all tests - ([216af73](https://github.com/ElaraLang/elara/commit/216af73edfeb5da214999f961bcbc52bdc73591b)) 
- Fix gh workflow - ([143afe5](https://github.com/ElaraLang/elara/commit/143afe5adb1f55a401bcdd4cda9a545d12e5828f)) 
- Fix order of lambda params - ([34b992c](https://github.com/ElaraLang/elara/commit/34b992c8b58d478dbacf68e54398ecbaa7d384eb)) 
- Fix weird deveshell bug - ([018ad04](https://github.com/ElaraLang/elara/commit/018ad047f2573e477a51b63b0058a554ef5fcc7a)) 
- Fix Show instance for UniqueID - ([69abce6](https://github.com/ElaraLang/elara/commit/69abce637f21705340360d45bb5b247826c50aa9)) 
- Fix a bunch of warnings - ([ca5bada](https://github.com/ElaraLang/elara/commit/ca5badadb5429e8eea76dee44db34eb2aa4bbef0)) 
- Fix tests too - ([8ac8089](https://github.com/ElaraLang/elara/commit/8ac8089176bc12bea0878ab39e202a6a2252c289)) 
- Fix unix build - ([02d1936](https://github.com/ElaraLang/elara/commit/02d1936da432f7fe6c75bfe28c9898fab7e25509)) 
- Fix crlf endings breaking lexer - ([5c23b0c](https://github.com/ElaraLang/elara/commit/5c23b0cc250289fcd99b224df6f54fd34fd559a1)) 
- Fix unit tests - ([164dc28](https://github.com/ElaraLang/elara/commit/164dc284d4d83ecb063bd8208f036811f3f5fed9)) 
- Fix jvm separator on windows - ([59674d7](https://github.com/ElaraLang/elara/commit/59674d7c3bf625dc84c8176d43aa0034b198cbb7)) 
- Fix parsing of constructor patterns with args without parens - ([aadac49](https://github.com/ElaraLang/elara/commit/aadac49e00acf5d3e60b13bef214be081f38b919)) 
- Fix graph topology due from constructor patterns - ([5184f39](https://github.com/ElaraLang/elara/commit/5184f393679f5b66a4f3ac5357a5439382f3cfe8)) 
- Fix constructors with multiple fields being invalidly generated - ([6fd72ae](https://github.com/ElaraLang/elara/commit/6fd72ae0a78a4aabf87112f7e81741669f7b6a05)) 
- Fix wrong toString append being used - ([a668e0a](https://github.com/ElaraLang/elara/commit/a668e0a3530bc5a264e16a15a42ee58e6dabb4f0)) 
- Fix adt match calling in the wrong order - ([a396b22](https://github.com/ElaraLang/elara/commit/a396b224f317bd16b17ce88e849f5c1a4a2ce0ce)) 
- Fix type / kind inference and such - ([49fcb62](https://github.com/ElaraLang/elara/commit/49fcb624a679122d726b5c7b08685b86f6d4a127)) 
- Fix the prettyprinter changes breaking the unit tests - ([207856e](https://github.com/ElaraLang/elara/commit/207856e27346683d85001b2ad8cbb49c66d6b618)) 
- Fix code generation for constructor patterns with multiple arguments breaking - ([6de3e20](https://github.com/ElaraLang/elara/commit/6de3e203a2d6ac90b98e7aea835062189a717621)) 

### 🚜 Refactor

- *(stdlib)* Move ALL mentions of lists to the stdlib - they are no longer a primitive! - ([8e3d367](https://github.com/ElaraLang/elara/commit/8e3d367d94078552ea642851654c7abe7b7ff42e)) 
- *(stdlib)* Refactor Elara Stdlib and add list length function - ([2fa03be](https://github.com/ElaraLang/elara/commit/2fa03be4f9acc294bc2d0a41f5314fb9aceba625)) 
- *(stdlib)* [**breaking**] Change `readFile` to return String rather than EList<String> - ([01f4048](https://github.com/ElaraLang/elara/commit/01f40486c687c47d8f17a7795931d65c4f606e11)) 
- Refactor tests - ([8151cfd](https://github.com/ElaraLang/elara/commit/8151cfde6fec3c8cc106c20ce0ffc7885a507f63)) 
- Refactors - ([718aa8a](https://github.com/ElaraLang/elara/commit/718aa8aa8e3f4a33ede2375f3854c7a33e6948cc)) 
- Refactor HasDependencies to allow multiple keys - ([5d1490e](https://github.com/ElaraLang/elara/commit/5d1490e75e41ad492164850c909a7c96fb2c9619)) 
- Refactor how type constructors are handled in core - ([ba962be](https://github.com/ElaraLang/elara/commit/ba962be52873f8093166170256650c681b48c715)) 
- Slightly tidy up generation of lambdas - ([4654676](https://github.com/ElaraLang/elara/commit/4654676b73c2fa3258d9b3bf044f56e7a2f6e677)) 
- Begin work refactoring lambda generation - keep track of parameter names _eerywhere_ - ([9f8ed70](https://github.com/ElaraLang/elara/commit/9f8ed708d95cab1b5106606d15e96aa9d3aecd91)) 

### 📚 Documentation

- *(stdlib)* Remove mention of EList from readme compiling guide - ([624a156](https://github.com/ElaraLang/elara/commit/624a156569ef71567788a0d380ea13212dd5d9c5)) 
- Document the pipeline a bit - ([efd41ea](https://github.com/ElaraLang/elara/commit/efd41ea37b869d5249de26cf3eee44fb6176915c)) 

### 🎨 Styling

- *(actions)* Remove blank line - ([9b61673](https://github.com/ElaraLang/elara/commit/9b6167309e788bcaa4adc40493db610ef88a70c3)) 
- *(tocore)* Change weird usage of =<< in ToCore - ([936ab2a](https://github.com/ElaraLang/elara/commit/936ab2a3ec0b740fabae2c0954cc29d1f4cb4b24)) 

### 🧪 Testing

- *(type-infer)* Add new tests for type application insertion - ([a341c65](https://github.com/ElaraLang/elara/commit/a341c6597677d6ff814d73cd6896231e7ed3f8b4)) 
- *(type-infer)* Uncomment test for `forall a. a` tyapps - ([e2786a0](https://github.com/ElaraLang/elara/commit/e2786a056b7ab8ba46b96616cc953f6da3e84736)) 
- Test - ([dd1a893](https://github.com/ElaraLang/elara/commit/dd1a8930c50658d46009a6690db05244acd81532)) 

### ⚙️ Miscellaneous Tasks

- *(actions)* Try deleting this folder - ([fb8c8bd](https://github.com/ElaraLang/elara/commit/fb8c8bd56ee9b231b39e1004123883934913c0a6)) 
- *(actions)* Change the script...? - ([27dc8ea](https://github.com/ElaraLang/elara/commit/27dc8eaac5a23a955ae7336e96320b95663906d2)) 
- *(actions)* Run linux builds on self-hosted runner - ([e47fa1a](https://github.com/ElaraLang/elara/commit/e47fa1acbdc2e180788bd8c008331baaa665392a)) 
- *(actions)* Nevermind let's not do that - ([c684cb9](https://github.com/ElaraLang/elara/commit/c684cb95ab3c53c4a2508ccc8a8b676b5c6f7cc7)) 
- *(actions)* Log big files - ([e0158fd](https://github.com/ElaraLang/elara/commit/e0158fd46eb94fcaaf9df45b06dd4ceb8526e4fc)) 
- *(actions)* That seemed to work - now remove the logging - ([f9a9294](https://github.com/ElaraLang/elara/commit/f9a9294cc28589d3204c5c1e3c4e84f0a95764cf)) 
- *(logging)* Change back to info logging (really need to make this configurable at runtime) - ([7666ae9](https://github.com/ElaraLang/elara/commit/7666ae96126238a5f453a4462aedb692d0316093)) 
- *(nix)* Move from mission-control to just - ([ba41767](https://github.com/ElaraLang/elara/commit/ba4176752274ae6b6cdbc870d8bc1d808378380d)) 
- *(nix)* Add the old mission-control scripts back to the justfile - ([1508631](https://github.com/ElaraLang/elara/commit/15086319ecb703d2774a436775312f7409df0489)) 
- *(stdlib)* Remove random list from main file - ([85524a1](https://github.com/ElaraLang/elara/commit/85524a108d84963b0b7090093ffb952e9d4ff4f8)) 

### Build

- *(nix)* Update nix flake - ([f95de02](https://github.com/ElaraLang/elara/commit/f95de028cc2349f71901f8c5c7e7b0cd8e4b1558)) 

<!-- generated by git-cliff -->
