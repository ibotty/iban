{-# LANGUAGE OverloadedStrings #-}
module Finance.IBAN.Germany
  ( BIC
  , BLZ
  , AccountNr
  -- , bics
  , ibanFromLegacy
  , legacyFromIBAN
  ) where

import Control.Arrow (second)
import Data.Text (Text)
import Finance.IBAN.Internal
-- import Finance.IBAN.Germany.Data as D

import qualified Data.ISO3166_CountryCodes as CC
import qualified Data.Text as T

type BIC = Text
type BLZ = Text
type AccountNr = Text

ibanFromLegacy :: BLZ -> AccountNr -> IBAN
ibanFromLegacy blz account = IBAN $ ibanWithChecksum checksum
  where
    ibanWithChecksum c = T.concat [cc, c, blz, accountStr]
    accountStr = T.justifyRight 10 '0' account
    cc = T.pack $ show CC.DE
    ibanCandidate = ibanWithChecksum "00"
    checksum = T.pack $ case show (98 - mod97 ibanCandidate) of
                          [d,d'] -> [ d,  d']
                          [d']   -> ['0', d']

legacyFromIBAN :: IBAN -> (BLZ, AccountNr)
legacyFromIBAN = second (T.dropWhile (== '0')) . T.splitAt 8 . T.drop 4 . rawIBAN

-- bics :: [(BLZ, BIC)]
-- bics = undefined
