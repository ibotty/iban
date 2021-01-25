{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow
import Data.Either
import Finance.IBAN
import Finance.IBAN.Germany
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified IBANRegistryExamples as R
import Data.Text (Text, pack)


main :: IO ()
main = do
  defaultMain $ testGroup "all tests"
    [ testGroup "IBAN Registry Examples validate" registryTests
    , testGroup "German legacy account transformation" germanLegacyTests
    , testProperties "Check that IBAN parser:" 
      [ ("can handle arbitrary input", withMaxSuccess 10000 prop_can_handle_arbitrary_input)
      , ("can check arbitrary BBAN (checksum bug)", withMaxSuccess 10000 prop_can_check_arbitrary_bban)
      ]
    ]

registryTests = map mkTestCase R.examples
  where
    mkTestCase ex = testCase ("iban " ++ show ex) $ assertRight (parseIBAN ex)

germanLegacyTests =
  [ testGroup "generated ibans are valid"
        (zipWith generatedAreValid accountDetails ibans)
  , testGroup "legacyFromIBAN is somewhat inverse of ibanFromLegacy"
        (map legacyToFrom accountDetails)
  ]
  where
    generatedAreValid details iban = testCase ("iban " ++ show iban) $
        parseIBAN iban @=? Right (fst $ uncurry ibanFromLegacy details)


    legacyToFrom d@(blz, account) = testCase (show blz ++ " " ++ show account) $
        d @=? legacyFromIBAN (fst $ ibanFromLegacy blz account)

    accountDetails = [ ("37040044", "532013000")
                     ]
    ibans = ["DE 8937 0400 4405 3201 3000"]

assertRight :: Show a => Either a b -> Assertion
assertRight (Right _) = return ()
assertRight (Left a)  = assertFailure $ show a

-- basically, test that parser won't fail with `error`
prop_can_handle_arbitrary_input :: String -> Bool
prop_can_handle_arbitrary_input input =
  let result = parseIBAN (pack input)
  in  isRight result || isLeft result

-- basically, test that parser won't fail with `error`
prop_can_check_arbitrary_bban :: String -> Bool
prop_can_check_arbitrary_bban input =
  let result = parseIBAN (pack . ("CZ65 "++) $ input)
  in  isRight result || isLeft result
