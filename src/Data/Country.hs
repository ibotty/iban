{-# LANGUAGE DeriveGeneric, DeriveLift, DerivingStrategies, DeriveAnyClass #-}

module Data.Country (
  CountryCode (..),
  module Contrib.Data.ISO3166_CountryCodes
) where

import qualified Contrib.Data.ISO3166_CountryCodes as ISO3166
import Contrib.Data.ISO3166_CountryCodes hiding (CountryCode)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Data.Validity (Validity)
import Data.GenValidity (GenValid, genValid, shrinkValid)
import Text.Read
import Data.Char (toUpper)
import Control.Monad (guard)
import Test.QuickCheck (Arbitrary (..))


data CountryCode
  = ISO3166 ISO3166.CountryCode
  -- Private Codes, see https://en.wikipedia.org/wiki/ISO_3166#Codes_beginning_with_%22X%22
  | X Char
  deriving stock (Eq, Ord, Generic, Lift)
  deriving anyclass (Validity, GenValid)

instance Show CountryCode where
  show (ISO3166 x) = show x
  show (X c) = ['X', c]

instance Read CountryCode where 
  readPrec = private +++ (ISO3166 <$> readPrec)
    where
      private = do
        c1 <- get
        guard (toUpper c1 == 'X')
        c2 <- get
        pure (X (toUpper c2))

instance Arbitrary CountryCode where
  arbitrary = genValid
  shrink = shrinkValid