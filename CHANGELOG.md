# Changelog

All notable changes to this project will be documented in this file.

Note that while Elara follows SemVer where possible, most of the source code is not considered public API with regards to breaking changes.
As such, breaking changes may occur between minor versions until a stable release of the language.

## [unreleased]

### ⚙️ Miscellaneous Tasks

- *(changelog)* Add release script - ([8dca1bb](https://github.com/ElaraLang/elara/commit/8dca1bb1d6a90ac20d867fb056d54ac809d0b844)) 

## [0.1.0](https://github.com/ElaraLang/elara/compare/v0.0.2..vv0.1.0) - 2024-06-01

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

### Build

- *(changelog)* Add cliff.toml - ([682cc9f](https://github.com/ElaraLang/elara/commit/682cc9f771b49de35f2f4d70f4829d2e4bd31523)) 
- *(changelog)* Add cliff.toml - ([b498ae3](https://github.com/ElaraLang/elara/commit/b498ae36aaad575f2f375596fc67f8b6b4cfe713)) 
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
- Fix the tests
 - ([1273624](https://github.com/ElaraLang/elara/commit/1273624f5efd8a4dc9a52df073e199c03a180874)) 
- Fix name shadowing in inspection
 - ([56dc353](https://github.com/ElaraLang/elara/commit/56dc3534dfa22b5aee0582788784570c7560486e)) 
- Fix haddock comment
 - ([945cd39](https://github.com/ElaraLang/elara/commit/945cd398eaccb880d36017ba8f06279ba93880ae)) 
- Fix latex
 - ([57cb4ac](https://github.com/ElaraLang/elara/commit/57cb4ac62336eea9cddb65e24c730b70d3533801)) 
- Fix a bunch of warnings :D
 - ([bef63d7](https://github.com/ElaraLang/elara/commit/bef63d70b55ed5e988bd822513df2ed95c44bdde)) 
- Fix some more warnings
 - ([ea6b295](https://github.com/ElaraLang/elara/commit/ea6b295a211c57609665f2c23833578738d485a2)) 
- Fix more warnings
 - ([877fa2d](https://github.com/ElaraLang/elara/commit/877fa2df71692775c9cba46f92c361fae38b747b)) 
- Fix a naughty parser edge case, but now indentation is broken
 - ([fb64659](https://github.com/ElaraLang/elara/commit/fb646599f65db122aa30e5cea8a250e055de5429)) 
- Fix local let bindings not being annotated properly
 - ([5437b1b](https://github.com/ElaraLang/elara/commit/5437b1bc0f789b89090689919a0cda3e3f873684)) 
- Fix parsing tests, make let-binding names unqualified
 - ([16221a2](https://github.com/ElaraLang/elara/commit/16221a2b96fd9ddf7dc6e80b1523c8ae08376253)) 
- Fix funny quickcheck edge cases
 - ([c231e2a](https://github.com/ElaraLang/elara/commit/c231e2ac6872efe1d4aac5590087f6f01c0ea446)) 
- Fix some haddock typos
 - ([543a982](https://github.com/ElaraLang/elara/commit/543a982fb6c6c2bb9e288465ace248b64eb73ba0)) 
- Fix let-in error
 - ([9b60ec8](https://github.com/ElaraLang/elara/commit/9b60ec83ea45e08cfd022832c64c890a0f748a88)) 
- Fix prettyprint infinite loop lol
 - ([ace5c19](https://github.com/ElaraLang/elara/commit/ace5c19e65adf51e119efa3af67a99eaa8addc66)) 
- Fix unification with type declarations
 - ([24e2278](https://github.com/ElaraLang/elara/commit/24e2278773432462e072d1e412e530ac30d52026)) 
- Fix da nix
 - ([7ed19ef](https://github.com/ElaraLang/elara/commit/7ed19ef155b2f56e03544e06de7bdef3ac6aeee4)) 
- Fix da stack (doesnt rhyme sorry)
 - ([79018aa](https://github.com/ElaraLang/elara/commit/79018aabb9594ee3aef344f8b92c03fbba2b1583)) 
- Fix type inference?
 - ([b05c98a](https://github.com/ElaraLang/elara/commit/b05c98a9ff0afeabb47155c88ab6c1643253c3fd)) 
- Fix hie
 - ([2b53f88](https://github.com/ElaraLang/elara/commit/2b53f88ee07827d67cf0c7ccce5520ffeb38b7bc)) 
- Fix unit tests
 - ([2ef17dd](https://github.com/ElaraLang/elara/commit/2ef17dd60ebfabbec9f05a7b67fcfb2528e02db2)) 
- Fix doctests
 - ([f2543c7](https://github.com/ElaraLang/elara/commit/f2543c7bd40427ad91488df1dc959d6df58c8774)) 
- Fix the pattern parser & add a lot of tests
 - ([94d1669](https://github.com/ElaraLang/elara/commit/94d1669fa7c151d4d36964fa3a05ca185173cb1b)) 
- Fix lots of bugs!
 - ([438e69f](https://github.com/ElaraLang/elara/commit/438e69f744c68b730aa625e891e593d57a5aaff7)) 
- Fix subtyping on custom types
 - ([c790d2f](https://github.com/ElaraLang/elara/commit/c790d2f756aef3386a5f54ff8fd32898f11dbd27)) 
- Fix stupid ugly CI (maybe)
 - ([ad7a872](https://github.com/ElaraLang/elara/commit/ad7a87227ce7a506a5903c228ea390f17fb517e7)) 
- Fix some warnings and refactor even more
 - ([1d96b4d](https://github.com/ElaraLang/elara/commit/1d96b4d7d96379084a914f3d8090bf4de9078be1)) 
- Fix a morbillion more warnings
 - ([be1c891](https://github.com/ElaraLang/elara/commit/be1c891aadf9850bb817aa0aaca5216b1ba6568b)) 
- Fix almost all the warnings
 - ([351f7fb](https://github.com/ElaraLang/elara/commit/351f7fb55ed24254401eb88894cc790719a3069d)) 
- Fix a very rare lexer edge case
 - ([b0369fc](https://github.com/ElaraLang/elara/commit/b0369fcc435934d20876ae0dd08e0c165acc8e11)) 
- Fix build error
 - ([c154b25](https://github.com/ElaraLang/elara/commit/c154b25657399e75d675761e97f0baf597714dbb)) 
- Fix the tests not compiling (oops)
 - ([4a620ee](https://github.com/ElaraLang/elara/commit/4a620ee9f3549ce4592ca06a88cfe8f57b9497a4)) 
- Fix the tests not compiling (oops)
 - ([4ff8661](https://github.com/ElaraLang/elara/commit/4ff8661e822452390a0ebc402a7055c152a638f8)) 
- Fix let-in constructs being given the wrong type in core
 - ([e52396e](https://github.com/ElaraLang/elara/commit/e52396e44274be57da41a52fe7189040cbd40a57)) 
- Fix maxLocals for <clinit>
 - ([1a09f30](https://github.com/ElaraLang/elara/commit/1a09f30c220e563ca60d2d610e7b7ab65838a580)) 
- Fix bug in renaming let statements
 - ([4825d10](https://github.com/ElaraLang/elara/commit/4825d102860a0a61c8dae696875b42cff7eb6c44)) 
- Fix the cons type
 - ([8813151](https://github.com/ElaraLang/elara/commit/8813151a17505b3224514a429c93df1220e03302)) 
- Fix parse error with higher order functions
 - ([4452451](https://github.com/ElaraLang/elara/commit/44524515c89c7a84dd3578228fee0c609a539b04)) 
- Fix unnecessary type applications being added
 - ([09dc4ba](https://github.com/ElaraLang/elara/commit/09dc4ba5e96d93dc09cebe79a170815b0c479d4a)) 
- Fix variables in ToCore using the wrong types
 - ([d3da4bb](https://github.com/ElaraLang/elara/commit/d3da4bb60b3ee3e376b19939fa067a85819aae95)) 
- Fix (?) type inference for list literals
 - ([8a20d85](https://github.com/ElaraLang/elara/commit/8a20d856748c3472b488fa719b3c117074a5b07f)) 
- Fix CI
 - ([34bbcb0](https://github.com/ElaraLang/elara/commit/34bbcb0fe09c74121a723c2da76c6aff86e3887c)) 
- Fix undefined handling
 - ([85a0df4](https://github.com/ElaraLang/elara/commit/85a0df48b54f9cb6e76ff487150f840d0a4d1cdc)) 
- Fix |> and emitting higher order functions
 - ([69b3236](https://github.com/ElaraLang/elara/commit/69b3236708023b15ea9aed0b011d324bf7990ae5)) 
- Fix more tyapps
 - ([70aae06](https://github.com/ElaraLang/elara/commit/70aae06edb2a4ea2ebdcc31707532b1a68624ff3)) 
- Fix redundant pattern match warning with Local' & Global'
 - ([cd0ca0c](https://github.com/ElaraLang/elara/commit/cd0ca0c8b7982a9083c20ea2adfbfed25ad1a331)) 
- Fix core type instantiation being incorrect
 - ([97427ab](https://github.com/ElaraLang/elara/commit/97427ab1a7731e19bb4ccec1170c5f4350158e0a)) 
- Fix match expressions inferring to the wrong type
 - ([ad97cef](https://github.com/ElaraLang/elara/commit/ad97cefc9f91e5385977692e78b78b83974825d1)) 
- Fix ci running
 - ([a858457](https://github.com/ElaraLang/elara/commit/a8584571399ee3ff27f7fcae80546b012a5945eb)) 
- Fix shunt tests
 - ([d017534](https://github.com/ElaraLang/elara/commit/d017534f7c17767811bd2663d4167d2b8a30b782)) 
- Fix a bunch of warnings
 - ([aa811c7](https://github.com/ElaraLang/elara/commit/aa811c7ccc41b085aa58a78cdeb59bfbf57762fe)) 
- Fix the unit tests compiling
 - ([b2e838d](https://github.com/ElaraLang/elara/commit/b2e838dc9ee35e6a9f8abb7a9a01064904d1c682)) 
- Fix all tests
 - ([216af73](https://github.com/ElaraLang/elara/commit/216af73edfeb5da214999f961bcbc52bdc73591b)) 
- Fix gh workflow
 - ([143afe5](https://github.com/ElaraLang/elara/commit/143afe5adb1f55a401bcdd4cda9a545d12e5828f)) 
- Fix order of lambda params
 - ([34b992c](https://github.com/ElaraLang/elara/commit/34b992c8b58d478dbacf68e54398ecbaa7d384eb)) 
- Fix weird deveshell bug
 - ([018ad04](https://github.com/ElaraLang/elara/commit/018ad047f2573e477a51b63b0058a554ef5fcc7a)) 
- Fix Show instance for UniqueID
 - ([69abce6](https://github.com/ElaraLang/elara/commit/69abce637f21705340360d45bb5b247826c50aa9)) 
- Fix a bunch of warnings
 - ([ca5bada](https://github.com/ElaraLang/elara/commit/ca5badadb5429e8eea76dee44db34eb2aa4bbef0)) 
- Fix tests too
 - ([8ac8089](https://github.com/ElaraLang/elara/commit/8ac8089176bc12bea0878ab39e202a6a2252c289)) 
- Fix unix build
 - ([02d1936](https://github.com/ElaraLang/elara/commit/02d1936da432f7fe6c75bfe28c9898fab7e25509)) 
- Fix crlf endings breaking lexer
 - ([5c23b0c](https://github.com/ElaraLang/elara/commit/5c23b0cc250289fcd99b224df6f54fd34fd559a1)) 
- Fix unit tests
 - ([164dc28](https://github.com/ElaraLang/elara/commit/164dc284d4d83ecb063bd8208f036811f3f5fed9)) 
- Fix jvm separator on windows
 - ([59674d7](https://github.com/ElaraLang/elara/commit/59674d7c3bf625dc84c8176d43aa0034b198cbb7)) 
- Fix parsing of constructor patterns with args without parens
 - ([aadac49](https://github.com/ElaraLang/elara/commit/aadac49e00acf5d3e60b13bef214be081f38b919)) 
- Fix graph topology due from constructor patterns
 - ([5184f39](https://github.com/ElaraLang/elara/commit/5184f393679f5b66a4f3ac5357a5439382f3cfe8)) 
- Fix constructors with multiple fields being invalidly generated
 - ([6fd72ae](https://github.com/ElaraLang/elara/commit/6fd72ae0a78a4aabf87112f7e81741669f7b6a05)) 
- Fix wrong toString append being used
 - ([a668e0a](https://github.com/ElaraLang/elara/commit/a668e0a3530bc5a264e16a15a42ee58e6dabb4f0)) 
- Fix adt match calling in the wrong order
 - ([a396b22](https://github.com/ElaraLang/elara/commit/a396b224f317bd16b17ce88e849f5c1a4a2ce0ce)) 
- Fix type / kind inference and such
 - ([49fcb62](https://github.com/ElaraLang/elara/commit/49fcb624a679122d726b5c7b08685b86f6d4a127)) 
- Fix the prettyprinter changes breaking the unit tests
 - ([207856e](https://github.com/ElaraLang/elara/commit/207856e27346683d85001b2ad8cbb49c66d6b618)) 
- Fix code generation for constructor patterns with multiple arguments breaking - ([6de3e20](https://github.com/ElaraLang/elara/commit/6de3e203a2d6ac90b98e7aea835062189a717621)) 

### 🚜 Refactor

- *(stdlib)* Move ALL mentions of lists to the stdlib - they are no longer a primitive! - ([8e3d367](https://github.com/ElaraLang/elara/commit/8e3d367d94078552ea642851654c7abe7b7ff42e)) 
- *(stdlib)* Refactor Elara Stdlib and add list length function - ([2fa03be](https://github.com/ElaraLang/elara/commit/2fa03be4f9acc294bc2d0a41f5314fb9aceba625)) 
- *(stdlib)* [**breaking**] Change `readFile` to return String rather than EList<String> - ([01f4048](https://github.com/ElaraLang/elara/commit/01f40486c687c47d8f17a7795931d65c4f606e11)) 
- Refactor tests
 - ([8151cfd](https://github.com/ElaraLang/elara/commit/8151cfde6fec3c8cc106c20ce0ffc7885a507f63)) 
- Refactors
 - ([718aa8a](https://github.com/ElaraLang/elara/commit/718aa8aa8e3f4a33ede2375f3854c7a33e6948cc)) 
- Refactor HasDependencies to allow multiple keys
 - ([5d1490e](https://github.com/ElaraLang/elara/commit/5d1490e75e41ad492164850c909a7c96fb2c9619)) 
- Refactor how type constructors are handled in core
 - ([ba962be](https://github.com/ElaraLang/elara/commit/ba962be52873f8093166170256650c681b48c715)) 
- Slightly tidy up generation of lambdas - ([4654676](https://github.com/ElaraLang/elara/commit/4654676b73c2fa3258d9b3bf044f56e7a2f6e677)) 
- Begin work refactoring lambda generation - keep track of parameter names _eerywhere_ - ([9f8ed70](https://github.com/ElaraLang/elara/commit/9f8ed708d95cab1b5106606d15e96aa9d3aecd91)) 

### 📚 Documentation

- *(stdlib)* Remove mention of EList from readme compiling guide - ([624a156](https://github.com/ElaraLang/elara/commit/624a156569ef71567788a0d380ea13212dd5d9c5)) 
- Document the pipeline a bit
 - ([efd41ea](https://github.com/ElaraLang/elara/commit/efd41ea37b869d5249de26cf3eee44fb6176915c)) 

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
