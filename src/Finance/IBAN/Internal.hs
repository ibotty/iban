{-# LANGUAGE OverloadedStrings #-}
module Finance.IBAN.Internal
  ( prettyIBAN
  , IBAN()
  ) where

import           Data.Char (digitToInt, isAlphaNum, isDigit, isLower, isUpper, toUpper)
import           Data.Either (either)
import           Data.Function.Pointless ((.:))
-- import           Data.ISO3166_CountryCodes (CountryCode)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
-- import           Finance.IBAN.Data
import           Text.Read

data IBAN = IBAN {rawIBAN :: Text}
  deriving (Eq)

instance IsString IBAN where
    fromString iban = either (error . show) id $ parseIBAN $ T.pack iban

instance Show IBAN where
    showsPrec p iban = showParen (p>10) $
        showString "fromString " . shows (prettyIBAN iban)

instance Read IBAN where
    readPrec = parens $ prec 10 $ do
        Ident "fromString" <- lexP
        str <- readPrec
        return (fromString str)

data IBANError = IBANInvalidCharacters
               | IBANInvalidStructure Text
               | IBANWrongChecksum
               | IBANInvalidCountry Text
               | IBANInvalid
  deriving (Show, Eq)

data Count = Fixed Int | Maximum Int
  deriving (Show, Eq)

data SElement = Numbers Count
              | Upper Count
              | Chars Count
              | Empty
  deriving (Show, Eq)

data IBANStructure = IBANStructure CountryCode SElement SElement SElement
  deriving (Show, Eq)

prettyIBAN :: IBAN -> Text
prettyIBAN (IBAN str) = T.intercalate " " $ T.chunksOf 4 str

parseIBAN :: Text -> Either Text IBAN
parseIBAN str
  | wrongChars = Left IBANInvalidCharacters
  -- | wrongStructure = Left $ IBANInvalidStructure wrongStructurePart
  | wrongChecksum = Left IBANWrongChecksum
  | otherwise = Right $ IBAN str'
  where str'          = T.filter (not . (== ' ')) str
        wrongChars    = T.any (not . isAlphaNum) str'
        wrongChecksum = 1 /= mod97 str'

-- | Calculate the reordered decimal number mod 97 using Horner's rule
mod97 :: Text -> Int
mod97 = fold . reorder
  where reorder = uncurry (flip T.append) . T.splitAt 4
        fold = T.foldl' (flip rem 97 .: add) 0
        add n c
          -- | is that right? all examples in the internet ignore lowercase
          | isLower c = add n $ toUpper c
          | isUpper c = 100*n + 10 + fromEnum c - fromEnum 'A'
          | isDigit c = 10*n + digitToInt c
          | otherwise = error $ "Finance.IBAN.Internal.mod97: wrong char " ++ [c]

constructIBAN :: Text -> Text -> Text -> Text -> Either IBANError IBAN
constructIBAN country bank branch account =
    Right . IBAN . T.concat $ [c, checksum, b, a]
  where ibanCandidate = T.concat [c, "00", b, a]
        checksum = case show (98 - mod97 ibanCandidate) of
                     [d,d'] -> T.pack [d,d']
                     [d]    -> T.pack [d]
