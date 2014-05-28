{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow
import Data.Either
import Finance.IBAN
import Finance.IBAN.Germany
import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit

import qualified IBANRegistryExamples as R
import qualified Data.Text as T

main :: IO ()
main = defaultMain $ testGroup "all tests"
  [ testGroup "IBAN Registry Examples validate" registryTests
  , testGroup "German legacy account transformation" germanLegacyTests
  ]

registryTests = map mkTestCase R.examples
  where
    mkTestCase ex = testCase ("iban " ++ show ex) $ assertRight (parseIBAN ex)

germanLegacyTests =
  [ testGroup "generated ibans are valid" (zipWith generatedAreValid accountDetails ibans)
  , testGroup "legacyFromIBAN . ibanFromLegacy == id" (map legacyToFrom accountDetails)
  ]
  where
    generatedAreValid details iban = testCase ("iban " ++ show iban) $
        parseIBAN iban @=? Right (uncurry ibanFromLegacy details)


    legacyToFrom d@(blz, account) = testCase (show blz ++ " " ++ show account) $
        d @=? legacyFromIBAN (ibanFromLegacy blz account)

    accountDetails = [ ("37040044", "532013000")
                     ]
    ibans = ["DE 8937 0400 4405 3201 3000"]


assertRight :: Show a => Either a b -> Assertion
assertRight (Right _) = return ()
assertRight (Left a)  = assertFailure $ show a
