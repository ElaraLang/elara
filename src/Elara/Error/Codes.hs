module Elara.Error.Codes where

import Elara.Data.Pretty

{- | A type for error codes.
Error codes should follow a standard format consisting of the following components:

@[E|W]@, followed by 4 digits. The symbol @E@ indicates an error, while @W@ indicates a warning.

The digits are used to identify the error or warning.

The first digit signals the stage in which the error was thrown:

- @0@: Lexer / Parser
- @1@: Desugaring and Renaming stage
- @2@: Operator Shunting Stage
- @3@: Typechecking Stage
- @4@: Code Generation Stage

The remaining digits are arbitrary and should be incremented for each new error or warning.
   They should be unique overall, but don't have to be unique within a stage or category. For example @E1001@, @E0001@, and @W1001@ can all exist at once.

Some errors are internal, rather than user-facing (i.e. something internal has gone wrong if they are thrown).
   These errors' codes should be prefixed with @[Internal]@.
-}
type ErrorCode = forall ann. Doc ann

fileReadError :: ErrorCode
fileReadError = "E0001"

tooMuchIndentation :: ErrorCode
tooMuchIndentation = "E0002"

genericParseError :: ErrorCode
genericParseError = "E0003"

unterminatedStringLiteral :: ErrorCode
unterminatedStringLiteral = "E0004"

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

partialNamesNotEqual :: ErrorCode
partialNamesNotEqual = "[Internal] E1007"

tooManyDeclarations :: ErrorCode
tooManyDeclarations = "E1008"

nonExistentModuleDeclaration :: ErrorCode
nonExistentModuleDeclaration = "E1009"

infixDeclarationWithoutValue :: ErrorCode
infixDeclarationWithoutValue = "E10010"

duplicateFixityAnnotations :: ErrorCode
duplicateFixityAnnotations = "E10011"

samePrecedence :: ErrorCode
samePrecedence = "E2001"

unknownPrecedence :: ErrorCode
unknownPrecedence = "W2001"

invokeStaticLocal :: ErrorCode
invokeStaticLocal = "E4001"

localVariableNotFound :: ErrorCode
localVariableNotFound = "E4002"
