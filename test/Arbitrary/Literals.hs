module Arbitrary.Literals where

import Test.QuickCheck
import Text.Show qualified as TS (Show(..)) 
import Text.Printf

newtype IntLiteral = IntLiteral {unIntLiteral :: Text}
  deriving (Show)

newtype FloatLiteral = FloatLiteral {unFloatLiteral :: Text}
  deriving (Show)

newtype StringLiteral = StringLiteral {unStringLiteral :: Text}

instance Show StringLiteral where
  show = toString . unStringLiteral

newtype CharLiteral = CharLiteral {unCharLiteral :: Text}
  deriving (Show)

instance Arbitrary IntLiteral where
    arbitrary = oneof [intLiteral, hexLiteral, octLiteral]
      where
        intLiteral = IntLiteral . show <$> arbitrary @Integer
        hexLiteral = IntLiteral . ("0x" <>) <$> arbitraryHexString
        octLiteral = IntLiteral . ("0o" <>) <$> arbitraryOctString

arbitraryHexString :: Gen Text
arbitraryHexString = fromString <$> listOf1 (oneof hexChars)
  where
    hexChars = pure <$> ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']

arbitraryOctString :: Gen Text
arbitraryOctString = fromString <$> listOf1 (oneof octChars)
  where
    octChars = pure <$> ['0', '1', '2', '3', '4', '5', '6', '7']

instance Arbitrary FloatLiteral where
    arbitrary = oneof [normalFloat, scientificIntFloat, scientificFloat]
      where
        normalFloat = FloatLiteral . show <$> arbitrary @Double
        scientificIntFloat = do
            p1 <- arbitrarySizedIntegral
            p2 <- arbitrarySizedNatural
            p3 <- arbitrarySizedIntegral
            
            pure (FloatLiteral (safelyPrintInt p1 <> "." <> show p2 <> "e" <> show p3))

        scientificFloat = do
            p1 <- arbitrarySizedIntegral
            p2 <- arbitrarySizedIntegral
            pure (FloatLiteral (safelyPrintInt p1 <> "e" <> show p2))

instance Arbitrary StringLiteral where
    arbitrary = do
        s <- show <$> listOf arbitraryPrintableChar
        pure (StringLiteral s)

instance Arbitrary CharLiteral where
    arbitrary = do
        c <- show <$> arbitraryPrintableChar
        pure (CharLiteral c)

safelyPrintInt :: Integer -> Text
safelyPrintInt d = fromString (printf "%d" d)