{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Finance.IBAN.Internal
  ( IBAN(..)
  , IBANError(..)
  , parseIBAN
  , prettyIBAN
  , SElement
  , country
  , checkStructure
  , parseStructure
  , countryStructures
  , mod97_10
  ) where

import           Control.Arrow (left)
import           Data.Char (digitToInt, isDigit, isAsciiLower, isAsciiUpper, toUpper)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.ISO3166_CountryCodes (CountryCode)
import           Data.List (foldl')
import           Data.Maybe (isNothing)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Finance.IBAN.Data as Data
import           Text.Read (Lexeme(Ident), Read(readPrec), parens, prec, readMaybe, readPrec, lexP)

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

-- | Get the country of the IBAN
country :: IBAN -> CountryCode
country = either err id . countryEither . rawIBAN
  where err = const $ error "IBAN.country: internal inconsistency"

-- | Parse the Country from a text IBAN
countryEither :: Text -> Either Text CountryCode
countryEither s = readNote' s $ T.take 2 s

data IBANError =
    IBANInvalidCharacters   -- ^ The IBAN string contains invalid characters.
  | IBANInvalidStructure    -- ^ The IBAN string has the wrong structure.
  | IBANWrongChecksum       -- ^ The checksum does not match.
  | IBANInvalidCountry Text -- ^ The country identifier is either not a
                            --   valid ISO3166-1 identifier or that country
                            --   does not issue IBANs.
  deriving (Show, Read, Eq, Typeable)

data SElement = SElement (Char -> Bool) Int Bool

type BBANStructure = [SElement]

-- | show a IBAN in 4-blocks
prettyIBAN :: IBAN -> Text
prettyIBAN (IBAN str) = T.intercalate " " $ T.chunksOf 4 str

-- | try to parse an IBAN
parseIBAN :: Text -> Either IBANError IBAN
parseIBAN str
  | wrongChars = Left IBANInvalidCharacters
  | wrongChecksum = Left IBANWrongChecksum
  | otherwise = do
                  country <- left IBANInvalidCountry $ countryEither s
                  structure <- note (IBANInvalidCountry $ T.take 2 s) $
                                    M.lookup country countryStructures
                  if checkStructure structure s
                    then Right $ IBAN s
                    else Left IBANInvalidStructure
  where
    s              = T.filter (/= ' ') str
    wrongChars     = T.any (not . Data.isCompliant) s
    wrongChecksum  = 1 /= mod97_10 s

checkStructure :: BBANStructure -> Text -> Bool
checkStructure structure s = isNothing $ foldl' step (Just s) structure
  where
    step :: Maybe Text -> SElement -> Maybe Text
    step Nothing _ = Nothing
    step (Just t) (SElement cond cnt strict) =
      case T.dropWhile cond t' of
        "" -> Just r
        r' -> if strict then Nothing
                        else Just $ r' <> r
      where
        (t', r) = T.splitAt cnt t

parseStructure :: Text -> (CountryCode, BBANStructure)
parseStructure completeStructure = (cc, structure)
  where
    (cc', s) = T.splitAt 2 completeStructure
    cc = either err id $ readNote' ("invalid country code" <> show cc') cc'

    structure = case T.foldl' step (0, False, []) s of
                  (0, False, xs) -> reverse xs
                  _              -> err "invalid"

    step :: (Int, Bool, [SElement]) -> Char -> (Int, Bool, [SElement])
    step (_,   True,   _ ) '!' = err "unexpected '!'"
    step (cnt, False,  xs) '!' = (cnt, True, xs)
    step (cnt, strict, xs)  c
      | isDigit c               = (cnt*10 + digitToInt c, False, xs)
      | c `elem` ("nace"::String) = addElement xs condition cnt strict
      | otherwise               = err $ "unexpected " ++ show c
      where
        condition = case c of
                      'n' -> isDigit
                      'a' -> isAsciiUpper
                      'c' -> \c' -> isAsciiUpper c' || isDigit c'
                      'e' -> (== ' ')
                      _   -> err $ "unexpected " ++ show c

    addElement xs repr cnt strict = (0, False, SElement repr cnt strict : xs)
    err details = error $ "IBAN.parseStructure: " <> details <> " in " <> show s

countryStructures :: Map CountryCode BBANStructure
countryStructures = M.fromList $ map parseStructure Data.structures

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

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

readNote' :: Read a => b -> Text -> Either b a
readNote' note = maybe (Left note) Right . readMaybe . T.unpack
