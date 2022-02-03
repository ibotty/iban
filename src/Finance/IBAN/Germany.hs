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
import Data.Either (fromRight)
import qualified Data.HashMap.Strict as HM
import qualified Data.ISO3166_CountryCodes as CC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Finance.IBAN.Germany.Core
import Finance.IBAN.Germany.Data
import Finance.IBAN.Internal

ibanFromLegacy :: BLZ -> AccountNr -> (IBAN, Maybe BIC)
ibanFromLegacy blz' account' =
  (fromRight countryStructureChangedErr (mkIBAN CC.DE bban), mBIC)
  where
    mBIC = HM.lookup blz blzBICs
    bban = BBAN {countryCode = CC.DE, unBban = [blz, accountStr]}
    accountStr = T.justifyRight 10 '0' account
    countryStructureChangedErr =
      error "ibanFromLegacy: unexpected error, did IBAN country structure for DE change?"
    filterNumbers = T.filter isDigit
    blz = filterNumbers blz'
    account = filterNumbers account'

legacyFromIBAN :: IBAN -> (BLZ, AccountNr)
legacyFromIBAN = second (T.dropWhile (== '0')) . T.splitAt 8 . T.drop 4 . toString
