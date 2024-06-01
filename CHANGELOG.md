# Changelog

All notable changes to this project will be documented in this file.

Note that while Elara follows SemVer where possible, most of the source code is not considered public API with regards to breaking changes.
As such, breaking changes may occur between minor versions until a stable release of the language.


<!-- 0 -->🚀 Features	
- :bug: Fix #14 and significantly improve error message quality for unknown names (d22a9b5)	
- :lipstick: Use code E0001 for unknown name again (8e4d3c0)	

<!-- 1 -->🐛 Bug Fixes	
- :rotating_light: Fix invalid import (9162419)	

<!-- 2 -->🚜 Refactor	
- :recycle: Change renamer to have the current module be _optional_ (515f619)	

<!-- 3 -->📚 Documentation	
- Update CHANGELOG.md (31c62a7)	
- Use the git-cliff format for current changelog (f52b80b)	
- Use the git-cliff format for current changelog (9356157)	

<!-- 5 -->🎨 Styling	
- :rotating_light: Remove unused variable binding (bbd9441)	
- :lipstick: Slightly reword unknown name error message (8936a7c)	

<!-- 6 -->🧪 Testing	
- Fix shunting tests no longer compiling (4e9d196)	

<!-- 7 -->⚙️ Miscellaneous Tasks	
- Add action to generate changelogs whenever you push to main (cea8bae)	
- Add action to generate changelogs whenever you push to main (bdbcfb9)	
- Add release script (8dca1bb)	

Build	
- Add `git-cliff` to devshell (8a6187e)	
- Add cliff.toml (682cc9f)	
- Add `git-cliff` to devshell (7b5a372)	
- Add cliff.toml (b498ae3)	
- Add a safety check to the release script and allow aborting it (c1ec919)	
- :bug: Add more safety checking to release.sh and fix weird sed behaviour (1d01db1)	
- :art: Change changelog generation for tags (536e580)	

<!-- 0 -->🚀 Features	
- Add a nice pretty error report for when the renamer catches ambiguous names (f31a6cd)	

<!-- 1 -->🐛 Bug Fixes	
- Fix heavy-handed ambiguous name checking reporting ambiguity when there actually isn't any! Fixes variable shadowing (ecb3481)	

<!-- 2 -->🚜 Refactor	
- Change renaming backend to allow multiple names with the same unqualified name (f61f155)	

<!-- 3 -->📚 Documentation	
- Update CHANGELOG.md (0882b33)	

<!-- 0 -->🚀 Features	
- Load all the stdlib files dynamically based on what's in the new stdlib folder (47e0e90)	
- Add Option module to stdlib (646f1ae)	
- Improve reporting of `UnknownConstructor` error in ToCore (8fda8b3)	

<!-- 1 -->🐛 Bug Fixes	
- Fix ToCore not being invoked in topological order, causing unknown name errors (8737b28)	

<!-- 2 -->🚜 Refactor	
- Move all stdlib files into new folder (e7cd134)	

<!-- 5 -->🎨 Styling	
- Make main file a bit more fully-fledged (32b919c)	
- Remove blank line (20b7d8a)	

<!-- 7 -->⚙️ Miscellaneous Tasks	
- Try deleting this folder (c35171c)	
- Change the script...? (4ab2c8f)	
- Run linux builds on self-hosted runner (9f8f291)	
- Nevermind let's not do that (a30d954)	
- Log big files (282fb34)	
- That seemed to work - now remove the logging (31478e6)	

<!-- 0 -->🚀 Features	
- Restore the stdlib to its former glo (0fc83e9)	
- Add primitive functions for working with strings (a582f06)	
- Improve colourisation of pretty-printer output (28492c5)	
- Remove the domain when printing foralls (as it will only ever be type) (2b5158e)	
- Determine the log level based on elaraDebug variable (93f8f8a)	
- Add a new error message in the case of multiple tyapps being detected (which shouldn't happen?) (7486ea1)	

<!-- 1 -->🐛 Bug Fixes	
- Fix the tests
 (1273624)	
- Fix name shadowing in inspection
 (56dc353)	
- Fix haddock comment
 (945cd39)	
- Fix latex
 (57cb4ac)	
- Fix a bunch of warnings :D
 (bef63d7)	
- Fix some more warnings
 (ea6b295)	
- Fix more warnings
 (877fa2d)	
- Fix a naughty parser edge case, but now indentation is broken
 (fb64659)	
- Fix local let bindings not being annotated properly
 (5437b1b)	
- Fix parsing tests, make let-binding names unqualified
 (16221a2)	
- Fix funny quickcheck edge cases
 (c231e2a)	
- Fix some haddock typos
 (543a982)	
- Fix let-in error
 (9b60ec8)	
- Fix prettyprint infinite loop lol
 (ace5c19)	
- Fix unification with type declarations
 (24e2278)	
- Fix da nix
 (7ed19ef)	
- Fix da stack (doesnt rhyme sorry)
 (79018aa)	
- Fix type inference?
 (b05c98a)	
- Fix hie
 (2b53f88)	
- Fix unit tests
 (2ef17dd)	
- Fix doctests
 (f2543c7)	
- Fix the pattern parser & add a lot of tests
 (94d1669)	
- Fix lots of bugs!
 (438e69f)	
- Fix subtyping on custom types
 (c790d2f)	
- Fix stupid ugly CI (maybe)
 (ad7a872)	
- Fix some warnings and refactor even more
 (1d96b4d)	
- Fix a morbillion more warnings
 (be1c891)	
- Fix almost all the warnings
 (351f7fb)	
- Fix a very rare lexer edge case
 (b0369fc)	
- Fix build error
 (c154b25)	
- Fix the tests not compiling (oops)
 (4a620ee)	
- Fix the tests not compiling (oops)
 (4ff8661)	
- Fix let-in constructs being given the wrong type in core
 (e52396e)	
- Fix maxLocals for <clinit>
 (1a09f30)	
- Fix bug in renaming let statements
 (4825d10)	
- Fix the cons type
 (8813151)	
- Fix parse error with higher order functions
 (4452451)	
- Fix unnecessary type applications being added
 (09dc4ba)	
- Fix variables in ToCore using the wrong types
 (d3da4bb)	
- Fix (?) type inference for list literals
 (8a20d85)	
- Fix CI
 (34bbcb0)	
- Fix undefined handling
 (85a0df4)	
- Fix |> and emitting higher order functions
 (69b3236)	
- Fix more tyapps
 (70aae06)	
- Fix redundant pattern match warning with Local' & Global'
 (cd0ca0c)	
- Fix core type instantiation being incorrect
 (97427ab)	
- Fix match expressions inferring to the wrong type
 (ad97cef)	
- Fix ci running
 (a858457)	
- Fix shunt tests
 (d017534)	
- Fix a bunch of warnings
 (aa811c7)	
- Fix the unit tests compiling
 (b2e838d)	
- Fix all tests
 (216af73)	
- Fix gh workflow
 (143afe5)	
- Fix order of lambda params
 (34b992c)	
- Fix weird deveshell bug
 (018ad04)	
- Fix Show instance for UniqueID
 (69abce6)	
- Fix a bunch of warnings
 (ca5bada)	
- Fix tests too
 (8ac8089)	
- Fix unix build
 (02d1936)	
- Fix crlf endings breaking lexer
 (5c23b0c)	
- Fix unit tests
 (164dc28)	
- Fix jvm separator on windows
 (59674d7)	
- Fix parsing of constructor patterns with args without parens
 (aadac49)	
- Fix graph topology due from constructor patterns
 (5184f39)	
- Fix constructors with multiple fields being invalidly generated
 (6fd72ae)	
- Fix wrong toString append being used
 (a668e0a)	
- Fix adt match calling in the wrong order
 (a396b22)	
- Fix type / kind inference and such
 (49fcb62)	
- Fix the prettyprinter changes breaking the unit tests
 (207856e)	
- Fix constructor pattern arguments being reversed in type inference (321fdd6)	
- Fix code generation for constructor patterns with multiple arguments breaking (6de3e20)	
- Fix conversion between types when explicitly applying type constructors (eg `def x : Option Int`) (940e960)	
- Fix constructor patterns not being completely solved (1dc7624)	
- Fix type inference logging _always_ being on debug (3b34bb1)	
- Fix unit tests not compiling for various reasons (e0a2955)	
- Fix if/else type inference not always unifying correctly and throwing errors (33a72c2)	
- Remove old debugPretty calls (64edc03)	
- Fix stringLength primitive not boxing integers (85fcf5a)	
- Improve algorithm for detecting tyapps, fixing some edge cases (8503c23)	

<!-- 2 -->🚜 Refactor	
- Refactor tests
 (8151cfd)	
- Refactors
 (718aa8a)	
- Refactor HasDependencies to allow multiple keys
 (5d1490e)	
- Refactor how type constructors are handled in core
 (ba962be)	
- Slightly tidy up generation of lambdas (4654676)	
- Begin work refactoring lambda generation - keep track of parameter names _eerywhere_ (9f8ed70)	
- Move ALL mentions of lists to the stdlib - they are no longer a primitive! (8e3d367)	
- Refactor Elara Stdlib and add list length function (2fa03be)	
- (breaking) Change `readFile` to return String rather than EList<String> (01f4048)	

<!-- 3 -->📚 Documentation	
- Document the pipeline a bit
 (efd41ea)	
- Remove mention of EList from readme compiling guide (624a156)	

<!-- 5 -->🎨 Styling	
- Change weird usage of =<< in ToCore (936ab2a)	
- Remove blank line (9b61673)	

<!-- 6 -->🧪 Testing	
- Test (dd1a893)	
- Add new tests for type application insertion (a341c65)	
- Uncomment test for `forall a. a` tyapps (e2786a0)	

<!-- 7 -->⚙️ Miscellaneous Tasks	
- Move from mission-control to just (ba41767)	
- Add the old mission-control scripts back to the justfile (1508631)	
- Change back to info logging (really need to make this configurable at runtime) (7666ae9)	
- Remove random list from main file (85524a1)	
- Try deleting this folder (fb8c8bd)	
- Change the script...? (27dc8ea)	
- Run linux builds on self-hosted runner (e47fa1a)	
- Nevermind let's not do that (c684cb9)	
- Log big files (e0158fd)	
- That seemed to work - now remove the logging (f9a9294)	

Build	
- Update nix flake (f95de02)	
<!-- generated by git-cliff -->
