{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Finance.IBAN.Internal
  ( IBAN (..),
    BBAN (..),
    IBANError (..),
    iban,
    parseIBAN,
    prettyIBAN,
    parseBBANByCountry,
    toString,
    mkIBAN,
  )
where

import Contrib.Data.ISO3166_CountryCodes (CountryCode)
import Control.Arrow (Arrow ((&&&)), left)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Bifunctor (bimap)
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, ord, toUpper)
import Data.Either (fromRight, isRight)
import Data.Function ((&))
import Data.GenValidity (GenValid (..), invalid)
import Data.GenValidity.Text ()
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as S
import Data.String (IsString, fromString)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Validity (Validity (..), declare, delve)
import Finance.IBAN.Data
  ( BBANStructure,
    IBANStructure (IBANStructure, bbanStructure),
    Len (..),
    Repr (..),
    StructElem (..),
    countryP,
    ibanStructureByCountry,
    toIBANElementP,
  )
import qualified Finance.IBAN.Data as Data
import Finance.IBAN.Germany.Core (ChecksumType)
import Finance.IBAN.Structure (isCompliant, lenPred, reprPred)
import GHC.Generics (Generic)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (lift))
import Test.QuickCheck (Arbitrary (..), Gen, choose, frequency, infiniteListOf, listOf1, suchThat, suchThatMap)
import Test.QuickCheck.Poly (B (unB))
import Text.Read
  ( Lexeme (Ident),
    Read (readPrec),
    lexP,
    parens,
    prec,
    readMaybe,
    readPrec,
  )

data IBAN = IBAN
  { code :: CountryCode,
    checkDigs :: CheckSum,
    getBban :: BBAN
  }
  deriving stock (Eq, Typeable, Lift, Generic)

instance Show IBAN where
  showsPrec p iban =
    showParen (p > 10) $
      showString "fromString " . shows (prettyIBAN iban)

instance Read IBAN where
  readPrec = parens $
    prec 10 $ do
      Ident "fromString" <- lexP
      either (error . show) id . parseIBAN . T.pack <$> readPrec

instance Validity IBAN where
  validate iban@IBAN {..} =
    maybe (invalid "country code") validateForStructure $
      ibanStructureByCountry code
    where
      validateForStructure structure =
        mconcat
          [ delve "the country code" code,
            declare "bban is correct for country" $ code == countryCode getBban,
            delve "the BBAN" getBban,
            delve "the checksum" checkDigs,
            declare "checksum is correct" $ checkDigs == calcChecksum code getBban
          ]

instance GenValid IBAN where
  genValid = do
    IBANStructure {countryCode = code, ..} <-
      genValid `suchThatMap` ibanStructureByCountry
    getBban <-
      (\unBban -> BBAN {countryCode = code, ..})
        <$> traverse genForStructure bbanStructure
    let checkDigs = calcChecksum code getBban
    pure IBAN {..}

instance Arbitrary IBAN where
  arbitrary = genValid
  shrink = shrinkValid

data BBAN = BBAN {countryCode :: CountryCode, unBban :: [Text]}
  deriving stock (Show, Eq, Typeable, Lift, Generic)

instance Validity BBAN where
  validate bban@BBAN {..} =
    maybe (invalid "country code") validateForStructure $
      ibanStructureByCountry countryCode
    where
      validateForStructure IBANStructure {..} =
        mconcat
          [ delve "the BBAN elements" unBban,
            declare ("correct number of elements for " <> show countryCode) $
              length bbanStructure == length unBban,
            foldMap validateElement (zip3 [1 ..] bbanStructure unBban)
          ]
      validateElement (i, StructElem {..}, x) =
        mconcat
          [ declare ("element " <> show i <> " is " <> descLen len <> " characters long") $
              lenPred len (T.length x),
            declare ("element " <> show i <> " consists of only " <> descRepr repr) $
              T.all (reprPred repr) x
          ]
      descLen (Fixed n) = show n
      descLen (Max n) = "up to " <> show n
      descRepr A = "uppercase letters"
      descRepr N = "digits"
      descRepr C = "alphanumerical characters"
      descRepr E = "only spaces"

instance GenValid BBAN where
  genValid = (genValid >>= genBBANForCountry) `suchThatMap` hasBBAN
    where
      hasBBAN = either (const Nothing) Just

genBBANForCountry :: CountryCode -> Gen (Either IBANError BBAN)
genBBANForCountry = traverse genBBAN . findIBANStructure
  where
    genBBAN IBANStructure {..} =
      (\unBban -> BBAN {..})
        <$> traverse genForStructure bbanStructure

genForStructure :: StructElem -> Gen Text
genForStructure = \case
  StructElem {len = Fixed n, ..} ->
    T.pack . take n <$> infiniteListOf (genForRepr repr)
  StructElem {len = Max n, ..} ->
    T.pack . take n <$> listOf1 (genForRepr repr)
  where
    genForRepr A = choose ('A', 'Z')
    genForRepr N = choose ('0', '9')
    genForRepr C =
      frequency
        [ (26, genForRepr A),
          (10, genForRepr N),
          (26, choose ('a', 'z'))
        ]
    genForRepr E = pure ' '

data CheckSum = CheckSum Char Char
  deriving stock (Show, Eq, Typeable, Lift, Generic)

instance Validity CheckSum where
  validate (CheckSum c10 c) =
    mconcat
      [ delve "the first digit" c10,
        delve "the second digit" c,
        declare "all digits are between 0 and 9" $ all isDigit [c10, c]
      ]

instance GenValid CheckSum where
  genValid = CheckSum <$> genDigit <*> genDigit
    where
      genDigit = choose ('0', '9')

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

-- | show a IBAN in 4-blocks
prettyIBAN :: IBAN -> Text
prettyIBAN = T.intercalate " " . T.chunksOf 4 . toString

toString :: IBAN -> Text
toString IBAN {..} = (T.pack . mconcat $ [show code, [c10, c]]) <> bbanToString getBban
  where
    (CheckSum c10 c) = checkDigs

bbanToString :: BBAN -> Text
bbanToString = mconcat . unBban

parseBBANByCountry :: CountryCode -> Text -> Either IBANError BBAN
parseBBANByCountry cCode txt = do
  s <- removeSpaces txt & validateChars
  bbanStruct <- findBBANStructure cCode
  _parseBBAN s cCode bbanStruct

-- | try to parse an IBAN
parseIBAN :: Text -> Either IBANError IBAN
parseIBAN str = do
  s <- removeSpaces str & validateChars >>= validateChecksum
  _countryCode <- parseCountryCode s
  struct <- findIBANStructure _countryCode
  _parseIBAN s struct

-- internal

_parseIBAN :: Text -> Data.IBANStructure -> Either IBANError IBAN
_parseIBAN s str = left (const InvalidIBANStructure) $ P.parseOnly (toIBANParser str) s

_parseBBAN :: Text -> CountryCode -> Data.BBANStructure -> Either IBANError BBAN
_parseBBAN txt countryCode str =
  left (const InvalidBBANStructure) $ P.parseOnly (toBBANParser countryCode str) txt

toIBANParser :: Data.IBANStructure -> Parser IBAN
toIBANParser Data.IBANStructure {..} = do
  _countryCode <- countryP
  _checkDigits <- CheckSum <$> P.digit <*> P.digit
  _bban <- toBBANParser _countryCode bbanStructure
  return $ IBAN _countryCode _checkDigits _bban

toBBANParser :: CountryCode -> Data.BBANStructure -> Parser BBAN
toBBANParser countryCode bbanStruct = do
  unBban <- traverse toIBANElementP bbanStruct
  P.endOfInput
  return $ BBAN {..}

toCheckDigitsP :: Data.StructElem -> Parser (Char, Char)
toCheckDigitsP se = do
  v <- toIBANElementP se
  maybe (fail "Error parsing check digits") pure (readMaybe $ unpack v)

parseCountryCode :: Text -> Either IBANError CountryCode
parseCountryCode = left (IBANInvalidCountry . T.pack) . P.parseOnly countryP

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
  if T.any (not . isCompliant) cs
    then Left InvalidCharacters
    else Right cs

validateChecksum :: Text -> Either IBANError Text
validateChecksum cs =
  if 1 /= mod97_10 cs
    then Left IBANWrongChecksum
    else Right cs

removeSpaces :: Text -> Text
removeSpaces = T.filter (/= ' ')

checksumAsInt :: CheckSum -> Int
checksumAsInt (CheckSum d10 d) = c2i d10 * 10 + c2i d
  where
    c2i x = ord x - ord '0'

intToChecksum :: Int -> Maybe CheckSum
intToChecksum x = case show x of
  [d] -> Just $ CheckSum '0' d
  [d, d'] -> Just $ CheckSum d d'
  _ -> Nothing

mkIBAN :: CountryCode -> BBAN -> Either IBANError IBAN
mkIBAN code bbanUnchecked = do
  getBban <- parseBBANByCountry code (bbanToString bbanUnchecked)
  let checkDigs = calcChecksum code getBban
  pure IBAN {..}

calcChecksum :: CountryCode -> BBAN -> CheckSum
calcChecksum cc bban = fromMaybe checksumToLarge $ intToChecksum checksumInt
  where
    candidate =
      IBAN
        { code = cc,
          checkDigs = CheckSum '0' '0',
          getBban = bban
        }
    checksumInt = 98 - mod97_10 (toString candidate)
    checksumToLarge = error "ibanFromLegacy: expected 0 <= mod97_10 x < 98"

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
