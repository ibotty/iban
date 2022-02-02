{-# LANGUAGE OverloadedStrings #-}

module Finance.IBAN.Germany
  ( BIC,
    BLZ,
    AccountNr,
    blzBICs,
    ibanFromLegacy,
    legacyFromIBAN,
  )
where

import Control.Arrow (second)
import Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import qualified Data.ISO3166_CountryCodes as CC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Finance.IBAN.Germany.Core
import Finance.IBAN.Germany.Data
import Finance.IBAN.Internal

ibanFromLegacy :: BLZ -> AccountNr -> (IBAN, Maybe BIC)
ibanFromLegacy blz' account' = (ibanWithChecksum checksum, mBIC)
  where
    mBIC = HM.lookup blz blzBICs
    ibanWithChecksum c =
      IBAN
        { code = CC.DE,
          checkDigs = c,
          getBban =
            BBAN
              { unBban = [blz, accountStr]
              }
        }
    accountStr = T.justifyRight 10 '0' account
    ibanCandidate = toString $ ibanWithChecksum ('0', '0')
    checksum = fromMaybe checksumBigger $ intToChecksum $ 98 - mod97_10 ibanCandidate
    checksumBigger = error "ibanFromLegacy: expected 0 <= mod97_10 x < 98"
    filterNumbers = T.filter isDigit
    blz = filterNumbers blz'
    account = filterNumbers account'

legacyFromIBAN :: IBAN -> (BLZ, AccountNr)
legacyFromIBAN = second (T.dropWhile (== '0')) . T.splitAt 8 . T.drop 4 . toString
