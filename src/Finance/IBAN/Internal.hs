{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Finance.IBAN.Internal
  ( IBAN(..)
  , IBANError(..)
  , parseIBAN
  , prettyIBAN
  , parseBBAN
  , parseBBANByCountry
  , mod97_10
  ) where

import           Control.Arrow (left)
import           Data.Char (digitToInt, isDigit, isAsciiLower, isAsciiUpper, toUpper)
import           Data.ISO3166_CountryCodes (CountryCode)
import           Data.String (IsString, fromString)
import           Data.Text (Text, unpack)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Finance.IBAN.Data as Data
import           Text.Read (Lexeme(Ident), Read(readPrec), parens, prec, readMaybe, readPrec, lexP)
import Data.Function ((&))
import Data.Attoparsec.Text as P
import Finance.IBAN.Data (toIBANElementP, countryP, uniqueBBANStructures)
import Data.Either (isRight)
import qualified Data.Set as S (foldr)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe, listToMaybe)

newtype IBAN = IBAN {rawIBAN :: Text}
  deriving (Eq, Typeable)

instance IsString IBAN where
    fromString iban = either (error . show) id $ parseIBAN $ T.pack iban

instance Show IBAN where
    showsPrec p iban = showParen (p>10) $
        showString "fromString " . shows (prettyIBAN iban)

instance Read IBAN where
    readPrec = parens $ prec 10 $ do
        Ident "fromString" <- lexP
        fromString <$> readPrec

newtype BBAN = BBAN {rawBBAN :: Text} deriving Show

data IBANError =
    CantValidateBBAN
  | NoIBANStructureFor CountryCode
  | NoBBANStructureFor CountryCode
  | IBANInvalidCharacters   -- ^ The IBAN string contains invalid characters.
  | InvalidIBANStructure    -- ^ The IBAN string has the wrong structure.
  | InvalidBBANStructure    -- ^ The BBAN string has the wrong structure.
  | IBANWrongChecksum       -- ^ The checksum does not match.
  | IBANInvalidCountry Text -- ^ The country identifier is either not a
                            --   valid ISO3166-1 identifier or that country
                            --   does not issue IBANs.
  deriving (Show, Read, Eq, Typeable)


-- | show a IBAN in 4-blocks
prettyIBAN :: IBAN -> Text
prettyIBAN (IBAN str) = T.intercalate " " $ T.chunksOf 4 str

newtype ValidatedBBAN = ValidatedBBAN {unBban :: [Text]} deriving Show
data ValidatedIBAN = ValidatedIBAN {code :: CountryCode, checkDigs :: Int, bban :: ValidatedBBAN} deriving Show

toString :: ValidatedIBAN -> Text
toString ValidatedIBAN{..} = (T.pack . mconcat $ [ show code , show checkDigs]) <> bbanText where
  bbanText :: Text
  bbanText = mconcat . unBban $ bban

-- | Try to parse BBAN with all available structures
parseBBAN :: Text -> Either IBANError BBAN
parseBBAN txt =  do
  s              <- removeSpaces txt & validateChars
  let foundValid = filter isRight $ S.foldr ((:) . _parseBBAN s) [] uniqueBBANStructures
  validatedBBAN  <- fromMaybe (Left CantValidateBBAN) (listToMaybe foundValid)
  return $ BBAN . mconcat . unBban $ validatedBBAN

parseBBANByCountry :: CountryCode -> Text -> Either IBANError BBAN
parseBBANByCountry cCode txt =  do
  s             <- removeSpaces txt & validateChars
  bbanStruct    <- findBBANStructure cCode
  validatedBBAN <- _parseBBAN s bbanStruct
  return $ BBAN . mconcat . unBban $ validatedBBAN

-- | try to parse an IBAN
parseIBAN :: Text -> Either IBANError IBAN
parseIBAN str = do
  s            <- removeSpaces str & validateChars >>= validateChecksum
  _countryCode <- parseCountryCode s
  struct       <- findIBANStructure _countryCode
  validIBAN    <- _parseIBAN s struct
  return $ IBAN (toString validIBAN)
  

-- internal

_parseIBAN :: Text -> Data.IBANStricture -> Either IBANError ValidatedIBAN
_parseIBAN s str = left (const InvalidIBANStructure) $ parseOnly (toIBANParser str) s

_parseBBAN :: Text -> Data.BBANStructure -> Either IBANError ValidatedBBAN
_parseBBAN txt str = left (const InvalidBBANStructure) $ parseOnly (toBBANParser str) txt

toIBANParser :: Data.IBANStricture -> Parser ValidatedIBAN
toIBANParser Data.IBANStricture{..} = do
  _countryCode <- countryP
  _checkDigits <- toCheckDigitsP checkDigitsStructure
  _bban        <- toBBANParser bbanStructure
  return $ ValidatedIBAN _countryCode _checkDigits _bban
  
toBBANParser :: Data.BBANStructure -> Parser ValidatedBBAN
toBBANParser bbanStruct =  do
    txt <- traverse toIBANElementP bbanStruct
    endOfInput
    return $ ValidatedBBAN txt

toCheckDigitsP :: Data.StructElem ->  Parser Int
toCheckDigitsP se = do
  v <- toIBANElementP se
  maybe (fail "Error parsing check digits") pure (readMaybe $ unpack v)
  
parseCountryCode :: Text -> Either IBANError CountryCode
parseCountryCode = left (IBANInvalidCountry . T.pack) . parseOnly countryP

findIBANStructure :: CountryCode -> Either IBANError Data.IBANStricture
findIBANStructure cc = case Data.ibanStructureByCountry cc of
                         Nothing         -> Left $ NoIBANStructureFor cc
                         Just ibanStruct -> Right ibanStruct

findBBANStructure :: CountryCode -> Either IBANError Data.BBANStructure
findBBANStructure cc = bimap
                          (const $ NoBBANStructureFor cc)
                          Data.bbanStructure
                          (findIBANStructure cc)

-- todo tests for validation
validateChars :: Text -> Either IBANError Text
validateChars cs = if T.any (not . Data.isCompliant) cs
                   then Left IBANInvalidCharacters
                   else Right cs

validateChecksum :: Text -> Either IBANError Text
validateChecksum cs = if 1 /= mod97_10 cs
                      then Left IBANWrongChecksum
                      else Right cs

removeSpaces :: Text -> Text
removeSpaces = T.filter (/= ' ')

-- | Calculate the reordered decimal number mod 97 using Horner's rule.
-- according to ISO 7064: mod97-10
mod97_10 :: Text -> Int
mod97_10 = fold . reorder
  where reorder = uncurry (flip T.append) . T.splitAt 4
        fold = T.foldl' ((flip rem 97 .) . add) 0
        add n c
          -- is that right? all examples in the internet ignore lowercase
          | isAsciiLower c = add n $ toUpper c
          | isAsciiUpper c = 100*n + 10 + fromEnum c - fromEnum 'A'
          | isDigit c      = 10*n + digitToInt c
          | otherwise      = error $ "Finance.IBAN.Internal.mod97: wrong char " ++ [c]
