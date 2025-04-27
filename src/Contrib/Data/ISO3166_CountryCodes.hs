{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Contrib.Data.ISO3166_CountryCodes
  ( module Data.ISO3166_CountryCodes,
  )
where

import Data.GenValidity (GenValid (genValid, shrinkValid))
import Data.ISO3166_CountryCodes
import Data.Validity (Validity)
import Language.Haskell.TH.Syntax (Lift)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))


deriving stock instance Lift CountryCode

deriving anyclass instance Validity CountryCode

deriving anyclass instance GenValid CountryCode

instance Arbitrary CountryCode where
  arbitrary = genValid
  shrink = shrinkValid
