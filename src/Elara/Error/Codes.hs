module Elara.Error.Codes where

{- | A type for error codes.
| While not enforced, error codes should follow a standard format consisting of the following components
| @[E|W]@, followed by 4 digits. The symbol @E@ indicates an error, while @W@ indicates a warning.
| The digits are used to identify the error or warning.
| The first digit signals the stage in which the error was thrown:
| @0@: Lexer / Parser
| @1@: Inspection & Annotating Stage
| @2@: Operator Shunting Stage
|
| The remaining digits are arbitrary and should be incremented for each new error or warning.
| They should be unique overall, but don't have to be unique within a stage or category. For example @E1001@, @E0001@, and @W1001@ can all exist at once.
-}
type ErrorCode = Text

genericParseError :: ErrorCode
genericParseError = "E0001"

unknownName :: ErrorCode
unknownName = "E1001"

unknownModule :: ErrorCode
unknownModule = "E1002"

ambiguousName :: ErrorCode
ambiguousName = "E1003"

qualifiedWithWrongModule :: ErrorCode
qualifiedWithWrongModule = "E1004"

duplicateDefinition :: ErrorCode
duplicateDefinition = "E1005"

defWithoutLet :: ErrorCode
defWithoutLet = "E1006"

tooManyDeclarations :: ErrorCode
tooManyDeclarations = "E1007"

samePrecedence :: ErrorCode
samePrecedence = "E2001"

unknownPrecedence :: ErrorCode
unknownPrecedence = "W2001"
