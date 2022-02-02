{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Finance.IBAN.Internal
  ( IBAN (..),
    BBAN (..),
    IBANError (..),
    iban,
    bban,
    parseIBAN,
    prettyIBAN,
    parseBBAN,
    parseBBANByCountry,
    mod97_10,
    toString,
    checksumAsInt,
    intToChecksum,
  )
where

import Contrib.Data.ISO3166_CountryCodes (CountryCode)
import Control.Arrow (left)
import Data.Attoparsec.Text as P
import Data.Bifunctor (bimap)
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, ord, toUpper)
import Data.Either (isRight)
import Data.Function ((&))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S (foldr)
import Data.String (IsString, fromString)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Finance.IBAN.Data (countryP, toIBANElementP, uniqueBBANStructures)
import qualified Finance.IBAN.Data as Data
import GHC.Generics (Generic)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (lift))
import Text.Read (Lexeme (Ident), Read (readPrec), lexP, parens, prec, readMaybe, readPrec)

iban :: QuasiQuoter
iban =
  QuasiQuoter
    { quoteExp = parseToExpression,
      quotePat = err,
      quoteType = err,
      quoteDec = err
    }
  where
    parseToExpression iban = either (fail . show) lift $ parseIBAN $ T.pack iban
    err _ = fail "[iban|...|] can only be used as expression."

instance Show IBAN where
  showsPrec p iban =
    showParen (p > 10) $
      showString "fromString " . shows (prettyIBAN iban)

instance Read IBAN where
  readPrec = parens $
    prec 10 $ do
      Ident "fromString" <- lexP
      either (error . show) id . parseIBAN . T.pack <$> readPrec

bban :: QuasiQuoter
bban =
  QuasiQuoter
    { quoteExp = parseToExpression,
      quotePat = err,
      quoteType = err,
      quoteDec = err
    }
  where
    parseToExpression iban = either (fail . show) lift $ parseBBAN $ T.pack iban
    err _ = fail "[bban|...|] can only be used as expression."

data IBANError
  = NoValidBBANStructureFound
  | NoIBANStructureFor CountryCode
  | NoBBANStructureFor CountryCode
  | -- | The IBAN string contains invalid characters.
    InvalidCharacters
  | -- | The IBAN string has the wrong structure.
    InvalidIBANStructure
  | -- | The BBAN string has the wrong structure.
    InvalidBBANStructure
  | -- | The checksum does not match.
    IBANWrongChecksum
  | -- | The country identifier is either not a
    --   valid ISO3166-1 identifier or that country
    --   does not issue IBANs.
    IBANInvalidCountry Text
  deriving (Show, Read, Eq, Typeable)

-- | show a IBAN in 4-blocks
prettyIBAN :: IBAN -> Text
prettyIBAN = T.intercalate " " . T.chunksOf 4 . toString

newtype BBAN = BBAN {unBban :: [Text]}
  deriving (Show, Eq, Typeable, Lift, Generic)

data IBAN = IBAN {code :: CountryCode, checkDigs :: (Char, Char), getBban :: BBAN}
  deriving (Eq, Typeable, Lift, Generic)

toString :: IBAN -> Text
toString IBAN {..} = (T.pack . mconcat $ [show code, [c10, c]]) <> bbanText
  where
    (c10, c) = checkDigs
    bbanText :: Text
    bbanText = mconcat . unBban $ getBban

-- | Try to parse BBAN with all available structures
parseBBAN :: Text -> Either IBANError BBAN
parseBBAN txt = do
  s <- removeSpaces txt & validateChars
  let foundValid = filter isRight $ S.foldr ((:) . _parseBBAN s) [] uniqueBBANStructures
  fromMaybe (Left NoValidBBANStructureFound) (listToMaybe foundValid)

parseBBANByCountry :: CountryCode -> Text -> Either IBANError BBAN
parseBBANByCountry cCode txt = do
  s <- removeSpaces txt & validateChars
  bbanStruct <- findBBANStructure cCode
  _parseBBAN s bbanStruct

-- | try to parse an IBAN
parseIBAN :: Text -> Either IBANError IBAN
parseIBAN str = do
  s <- removeSpaces str & validateChars >>= validateChecksum
  _countryCode <- parseCountryCode s
  struct <- findIBANStructure _countryCode
  _parseIBAN s struct

-- internal

_parseIBAN :: Text -> Data.IBANStructure -> Either IBANError IBAN
_parseIBAN s str = left (const InvalidIBANStructure) $ parseOnly (toIBANParser str) s

_parseBBAN :: Text -> Data.BBANStructure -> Either IBANError BBAN
_parseBBAN txt str = left (const InvalidBBANStructure) $ parseOnly (toBBANParser str) txt

toIBANParser :: Data.IBANStructure -> Parser IBAN
toIBANParser Data.IBANStructure {..} = do
  _countryCode <- countryP
  _checkDigits <- (,) <$> digit <*> digit
  _bban <- toBBANParser bbanStructure
  return $ IBAN _countryCode _checkDigits _bban

toBBANParser :: Data.BBANStructure -> Parser BBAN
toBBANParser bbanStruct = do
  txt <- traverse toIBANElementP bbanStruct
  endOfInput
  return $ BBAN txt

toCheckDigitsP :: Data.StructElem -> Parser (Char, Char)
toCheckDigitsP se = do
  v <- toIBANElementP se
  maybe (fail "Error parsing check digits") pure (readMaybe $ unpack v)

parseCountryCode :: Text -> Either IBANError CountryCode
parseCountryCode = left (IBANInvalidCountry . T.pack) . parseOnly countryP

findIBANStructure :: CountryCode -> Either IBANError Data.IBANStructure
findIBANStructure cc = case Data.ibanStructureByCountry cc of
  Nothing -> Left $ NoIBANStructureFor cc
  Just ibanStruct -> Right ibanStruct

findBBANStructure :: CountryCode -> Either IBANError Data.BBANStructure
findBBANStructure cc =
  bimap
    (const $ NoBBANStructureFor cc)
    Data.bbanStructure
    (findIBANStructure cc)

-- todo tests for validation
validateChars :: Text -> Either IBANError Text
validateChars cs =
  if T.any (not . Data.isCompliant) cs
    then Left InvalidCharacters
    else Right cs

validateChecksum :: Text -> Either IBANError Text
validateChecksum cs =
  if 1 /= mod97_10 cs
    then Left IBANWrongChecksum
    else Right cs

removeSpaces :: Text -> Text
removeSpaces = T.filter (/= ' ')

checksumAsInt :: (Char, Char) -> Int
checksumAsInt (d10, d) = c2i d10 * 10 + c2i d
  where
    c2i x = ord x - ord '0'

intToChecksum :: Int -> Maybe (Char, Char)
intToChecksum x = case show x of
  [d] -> Just ('0', d)
  [d, d'] -> Just (d, d')
  _ -> Nothing

-- | Calculate the reordered decimal number mod 97 using Horner's rule.
-- according to ISO 7064: mod97-10
mod97_10 :: Text -> Int
mod97_10 = fold . reorder
  where
    reorder = uncurry (flip T.append) . T.splitAt 4
    fold = T.foldl' ((flip rem 97 .) . add) 0
    add n c
      -- is that right? all examples in the internet ignore lowercase
      | isAsciiLower c = add n $ toUpper c
      | isAsciiUpper c = 100 * n + 10 + fromEnum c - fromEnum 'A'
      | isDigit c = 10 * n + digitToInt c
      | otherwise = error $ "Finance.IBAN.Internal.mod97: wrong char " ++ [c]
