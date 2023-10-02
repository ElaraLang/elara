module Elara.Parse.Literal (charLiteral, stringLiteral, integerLiteral, floatLiteral, unitLiteral) where

import Elara.Lexer.Token (Token (..))
import Elara.Parse.Primitives (Parser, satisfyMap, token_)

charLiteral :: Parser Char
charLiteral = satisfyMap $ \case
    TokenChar i -> Just i
    _ -> Nothing

stringLiteral :: Parser Text
stringLiteral = satisfyMap $ \case
    TokenString i -> Just i
    _ -> Nothing

integerLiteral :: Parser Integer
integerLiteral = satisfyMap $ \case
    TokenInt i -> Just i
    _ -> Nothing

floatLiteral :: Parser Double
floatLiteral = satisfyMap $ \case
    TokenFloat i -> Just i
    _ -> Nothing

unitLiteral :: Parser ()
unitLiteral = token_ TokenLeftParen <* token_ TokenRightParen
