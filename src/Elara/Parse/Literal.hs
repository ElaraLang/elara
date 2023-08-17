module Elara.Parse.Literal (charLiteral, stringLiteral, integerLiteral, floatLiteral, unitLiteral) where

import Elara.Lexer.Token (Token (..))
import Elara.Parse.Primitives (HParser, satisfyMap, token_)

charLiteral :: HParser Char
charLiteral = satisfyMap $ \case
    TokenChar i -> Just i
    _ -> Nothing

stringLiteral :: HParser Text
stringLiteral = satisfyMap $ \case
    TokenString i -> Just i
    _ -> Nothing

integerLiteral :: HParser Integer
integerLiteral = satisfyMap $ \case
    TokenInt i -> Just i
    _ -> Nothing

floatLiteral :: HParser Double
floatLiteral = satisfyMap $ \case
    TokenFloat i -> Just i
    _ -> Nothing

unitLiteral :: HParser ()
unitLiteral = (token_ TokenLeftParen <* token_ TokenRightParen)
