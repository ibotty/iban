{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow
import Data.Either
import Data.Text (Text, pack, unpack)
import Finance.IBAN
import Finance.IBAN.Germany
import qualified IBANRegistryExamples as R
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = do
  defaultMain $
    testGroup
      "all tests"
      [ testGroup "IBAN Registry Examples validate" ibanRegistryTests,
        testGroup "BBAN Registry Examples validate" bbanRegistryTests,
        testGroup "IBAN Bad Examples validate" badIbanRegistryTests,
        testGroup "BBAN Bad Examples validate" badBbanRegistryTests,
        testGroup "German legacy account transformation" germanLegacyTests,
        testProperties
          "Check that IBAN parser:"
          [ ("can handle arbitrary input", withMaxSuccess 10000 prop_can_handle_arbitrary_input),
            ("can check arbitrary BBAN (checksum bug)", withMaxSuccess 10000 prop_can_check_arbitrary_bban)
          ]
      ]

ibanRegistryTests = map mkTestCase R.ibanExamples
  where
    mkTestCase ex = testCase ("iban " ++ show ex) $ assertRight (parseIBAN ex)

bbanRegistryTests = map mkTestCase R.bbanExamples
  where
    mkTestCase ex = testCase ("bban " ++ show ex) $ assertRight (parseBBAN ex)

badIbanRegistryTests = map mkTestCase R.badIBANS
  where
    mkTestCase ex =
      let result = parseIBAN ex
       in testCase ("bad iban " ++ show result ++ " for: " ++ unpack ex) $ assertLeft result

badBbanRegistryTests = map mkTestCase R.badBBANS
  where
    mkTestCase ex =
      let result = parseBBAN ex
       in testCase ("bad bban " ++ show result ++ " for: " ++ unpack ex) $ assertLeft result

germanLegacyTests =
  [ testGroup
      "generated ibans are valid"
      (zipWith generatedAreValid accountDetails ibans),
    testGroup
      "legacyFromIBAN is somewhat inverse of ibanFromLegacy"
      (map legacyToFrom accountDetails)
  ]
  where
    generatedAreValid details iban =
      testCase ("iban " ++ show iban) $
        parseIBAN iban @=? Right (fst $ uncurry ibanFromLegacy details)

    legacyToFrom d@(blz, account) =
      testCase (show blz ++ " " ++ show account) $
        d @=? legacyFromIBAN (fst $ ibanFromLegacy blz account)

    accountDetails =
      [ ("37040044", "532013000")
      ]
    ibans = ["DE 8937 0400 4405 3201 3000"]

assertRight :: Show a => Either a b -> Assertion
assertRight (Right _) = return ()
assertRight (Left a) = assertFailure $ show a

assertLeft :: Show a => Either b a -> Assertion
assertLeft (Left _) = return ()
assertLeft (Right a) = assertFailure $ "Should fail, instead got: " ++ show a

-- basically, test that parser won't fail with `error`
prop_can_handle_arbitrary_input :: String -> Bool
prop_can_handle_arbitrary_input input =
  let result = parseIBAN (pack input)
   in isRight result || isLeft result

-- basically, test that parser won't fail with `error`
prop_can_check_arbitrary_bban :: String -> Bool
prop_can_check_arbitrary_bban input =
  let result = parseBBAN (pack input)
   in isRight result || isLeft result
