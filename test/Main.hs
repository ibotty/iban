module Main (main) where

import Finance.IBAN.Internal

import qualified IBANRegistryExamples as R

import Control.Arrow
import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit

main :: IO ()
main = defaultMain $ testCase "IBAN Registry Examples validate" registryTest

registryTest = [] @?= (filter (either (const True) (const False) . fst) $ map (parseIBAN &&& id) R.examples)

