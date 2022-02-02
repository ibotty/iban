{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Contrib.Data.ISO3166_CountryCodes
  ( module Data.ISO3166_CountryCodes,
  )
where

import Data.ISO3166_CountryCodes
import Language.Haskell.TH.Syntax (Lift)

deriving instance Lift CountryCode