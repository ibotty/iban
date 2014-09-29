{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Finance.IBAN.Germany
  ( BIC
  , BLZ
  , AccountNr
  , blzBICs
  , ibanFromLegacy
  , legacyFromIBAN
  ) where

import Control.Arrow (second)
import Data.Char (isDigit)
import Data.Text (Text)
import Finance.IBAN.Internal
import Finance.IBAN.Germany.Core
import Finance.IBAN.Germany.Data

import qualified Data.ISO3166_CountryCodes as CC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

ibanFromLegacy :: BLZ -> AccountNr -> (IBAN, Maybe BIC)
ibanFromLegacy blz' account' = (IBAN (ibanWithChecksum checksum), mBIC)
  where
    mBIC = HM.lookup blz blzBICs
    ibanWithChecksum c = T.concat [cc, c, blz, accountStr]
    accountStr = T.justifyRight 10 '0' account
    cc = T.pack $ show CC.DE
    ibanCandidate = ibanWithChecksum "00"
    checksum = T.pack $ case show (98 - mod97_10 ibanCandidate) of
                          [d,d'] -> [ d,  d']
                          [d']   -> ['0', d']
    filterNumbers = T.filter isDigit
    blz = filterNumbers blz'
    account = filterNumbers account'

legacyFromIBAN :: IBAN -> (BLZ, AccountNr)
legacyFromIBAN = second (T.dropWhile (== '0')) . T.splitAt 8 . T.drop 4 . rawIBAN
