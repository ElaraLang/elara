module Elara.Error.Codes where

type ErrorCode = Text

genericParseError :: ErrorCode
genericParseError = "E0001"

unknownName :: ErrorCode
unknownName = "E0002"

unknownModule :: ErrorCode
unknownModule = "E0003"