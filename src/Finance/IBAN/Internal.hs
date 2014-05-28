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
  , mod97
  ) where

import           Control.Arrow (left)
import           Data.Char (digitToInt, isAlphaNum, isDigit, isAsciiLower, isAsciiUpper, toUpper)
import           Data.Either (either)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.ISO3166_CountryCodes (CountryCode)
import           Data.List (foldl')
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid ((<>))
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Finance.IBAN.Data as Data
import           Text.Read (Lexeme(Ident), Read(readPrec), parens, prec, readMaybe, readPrec, lexP)

data IBAN = IBAN {rawIBAN :: Text}
  deriving (Eq, Typeable)

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
               | IBANInvalidStructure
               | IBANWrongChecksum
               | IBANInvalidCountry Text
  deriving (Show, Eq)

data CharRepr = Digits | UpperLetters | AlphaNums | Blanks
  deriving (Show, Eq)

data SElement = SElement (Char -> Bool) Int Bool

type IBANStructure = [SElement]

prettyIBAN :: IBAN -> Text
prettyIBAN (IBAN str) = T.intercalate " " $ T.chunksOf 4 str

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
    s              = T.filter (not . (== ' ')) str
    wrongChars     = T.any (not . isAlphaNum) s
    wrongChecksum  = 1 /= mod97 s

country :: IBAN -> CountryCode
country = either err id . countryEither . rawIBAN
  where err = const $ error "IBAN.country: internal inconsistency"

countryEither :: Text -> Either Text CountryCode
countryEither s = readNote' s $ T.take 2 s

checkStructure :: IBANStructure -> Text -> Bool
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

parseStructure :: Text -> (CountryCode, IBANStructure)
parseStructure completeStructure = (cc, structure)
  where
    (cc', s) = T.splitAt 2 completeStructure
    cc = either err id $ readNote' ("invalid country code" <> show cc') cc'

    structure = case T.foldl' step (0, False, []) s of
                  (0, False, xs) -> reverse xs
                  otherwise -> err "invalid"

    step :: (Int, Bool, [SElement]) -> Char -> (Int, Bool, [SElement])
    step (_,   True,   _ ) '!' = err "unexpected '!'"
    step (cnt, False,  xs) '!' = (cnt, True, xs)
    step (cnt, strict, xs)  c
      | isDigit c              = (cnt*10 + digitToInt c, False, xs)
      | c `elem` "nace"        = addElement xs condition cnt strict
      | otherwise              = err $ "unexpected " ++ show c
      where
        condition = case c of
                      'n' -> isDigit
                      'a' -> isAsciiUpper
                      'c' -> \c' -> isAsciiUpper c' || isDigit c'
                      'e' -> (== ' ')

    addElement xs repr cnt strict = (0, False, SElement repr cnt strict : xs)
    err details = error $ "IBAN.parseStructure: " <> details <> " in " <> show s

countryStructures :: Map CountryCode IBANStructure
countryStructures = M.fromList $ map parseStructure Data.structures

-- | Calculate the reordered decimal number mod 97 using Horner's rule
mod97 :: Text -> Int
mod97 = fold . reorder
  where reorder = uncurry (flip T.append) . T.splitAt 4
        fold = T.foldl' ((flip rem 97 .) . add) 0
        add n c
          -- | is that right? all examples in the internet ignore lowercase
          | isAsciiLower c = add n $ toUpper c
          | isAsciiUpper c = 100*n + 10 + fromEnum c - fromEnum 'A'
          | isDigit c      = 10*n + digitToInt c
          | otherwise      = error $ "Finance.IBAN.Internal.mod97: wrong char " ++ [c]

-- constructIBAN :: Text -> Text -> Text -> Text -> Either IBANError IBAN
-- constructIBAN country bank branch account =
--     Right . IBAN . T.concat $ [c, checksum, b, a]
--   where ibanCandidate = T.concat [c, "00", b, a]
--         checksum = case show (98 - mod97 ibanCandidate) of
--                      [d,d'] -> T.pack [d,d']
--                      [d]    -> T.pack [d]

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

readNote' :: Read a => b -> Text -> Either b a
readNote' note = maybe (Left note) Right . readMaybe . T.unpack
