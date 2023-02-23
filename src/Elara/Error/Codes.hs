module Elara.Error.Codes where

type ErrorCode = Text

genericParseError :: ErrorCode
genericParseError = "E0001"

unknownName :: ErrorCode
unknownName = "E0002"

unknownModule :: ErrorCode
unknownModule = "E0003"

ambiguousName :: ErrorCode
ambiguousName = "E0004"

qualifiedWithWrongModule :: ErrorCode
qualifiedWithWrongModule = "E0005"